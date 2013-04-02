(ns ced.main
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.java.shell :as sh]
            [clojure.string :as s]
            [clojure.walk :as w])
  (:import [java.io ByteArrayOutputStream PrintStream StringReader File Reader]
           [clojure.lang Reflector IDeref Ratio]
           [org.anarres.cpp Preprocessor PreprocessorListener CppReader
            Feature Warning LexerSource StringLexerSource FileLexerSource]
           [xtc.lang C]
           [xtc.parser ParseError]
           [xtc.tree Node])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn ^LexerSource lexer-source [file]
  (if (and (not (.exists (io/file file))) (string? file))
    (StringLexerSource. file true)
    (FileLexerSource. (io/file file))))

;; We want to get rid of JCPP if possible and bake the preprocessor into the main parse.
;; SuperC in xtc should be able to do this, but creates a huge, seemingly wrong, AST.
;; JCPP has a concept of virtual file systems, so we should be able to write a classloader based one.
(defn cpp [file]
  (let [pp (doto (Preprocessor.)
             (.addFeature Feature/DIGRAPHS);
             (.addFeature Feature/TRIGRAPHS);
             (.addWarning Warning/IMPORT);
             (.setListener (PreprocessorListener.))
             (.addMacro "__JCPP__")
             (.setSystemIncludePath ["resources/include"])
             (.addInput (lexer-source file)))]
    (slurp (CppReader. pp))))

(defn cpp-gcc [file]
  (:out (sh/sh "cpp" "-E" "-P" file)))

(defn man-short [fn]
  (second (re-find #"- (.+)\n" (:out (sh/sh "man" "-f" (str fn))))))

(defn man [fn]
  ;; 3   Library calls (functions within program libraries)
  (:out (sh/sh "man" "-P" "cat" "3" (str fn))))

(defn head [s & [n]]
  (s/join "\n" (take (or n 1) (s/split s #"\n"))))

(defn tail [s & [n]]
  (let [lines (s/split s #"\n")]
    (s/join "\n" (drop (- (count lines) (or n 1)) lines))))

(defn decamel [s]
  (s/lower-case
   (s/replace s
              #"(\p{Lower})?(\p{Upper})"
              (fn [[_ l u]] (str (when l (str l "-")) u)))))

(defn location [node]
  (let [location (.getLocation ^Node node)]
    (into {} (map (fn [f]
                    [f (Reflector/getInstanceField location (name f))])
                  [:line :column :file]))))

(defn parse-char [s]
  (when-let [[_ c] (re-find #"(?s)^'(.*)'$" s)]
    (first (reduce (fn [s [m r]] (s/replace s m r)) c
                   {#"\\n" "\n" "\\r" "\r" "\\t" "\t" "\\b" "\b"}))))

(defn ast->clj [node]
  (cond
   (instance? Node node) (with-meta
                           (cons
                            (keyword
                             (s/replace (decamel (.getName ^Node node)) #"^xtc.tree." ""))
                            (map ast->clj (seq node)))
                           {:location  (location node)
                            :node node})
   (string? node) (if-let [c (parse-char node)]
                    c
                    (read-string node))
   :else node))

;; Using Rats! C parser, decoupled from the xtc source tree (see src/xtc).
;; We could look into transforming the grammar, by hand or programatically, into some Clojure PEG parser.
;; Once the compiler itself works, we can use its structure to implement a pure Clojure parser.
(defn parse-c [source file]
  (let [parser (C. (StringReader. source) file)
        tu (.pTranslationUnit parser 0)]
    (if-not (.hasValue tu)
      (.signal parser (.parseError tu))
      (.semanticValue tu))))

(defn preprocess-and-parse-c [file]
  (let [file (io/file file)]
    (parse-c (cpp file) (.getName file))))

(defmulti compiler (fn [[type & args]] type))

(defmethod compiler nil [[_ & args]])

(defmethod compiler :default [[_ & args]]
  (println "don't know how to compile" _))

(defmethod compiler :empty-statement [[_ & args]])

(def bytes-class (type (byte-array 0)))

(defn char*-to-string [cs]
  (if (instance? bytes-class cs)
    (String. ^bytes cs)
    cs))

;; Fix all this crap, but just so we can have a stub for stdio a bit longer while dealing with core C.
;; I have yet to decide if we want to implement stdio as a namespace in Clojure, or compile stdio from musl's C.
;; Some low-level functions would still obviously have to be provided by Clojure.
(binding [*ns* (create-ns 'c)]
  (intern *ns* 'printf  (fn [^bytes format & args]
                          (count (doto (apply clojure.core/format
                                              (s/replace (char*-to-string format) #"%ld" "%d")
                                              (map char*-to-string args))
                                   print))))
  (intern *ns* 'putchar (fn ^long [^long c] (doto c (-> char print))))
  (intern *ns* 'getchar (fn ^long [] (.read ^Reader *in*)))
  (intern *ns* 'main))

(defn link [object]
  (binding [*ns* (the-ns 'c)]
    (eval object)))

(defmethod compiler :translation-unit [[_ & args]]
  (let [prelude? (first args)
        external-declaration* (next (butlast args))
        annotations (last args)
        tu (cons (compiler prelude?) (map compiler external-declaration*))]
    `(do ~@(remove nil? tu))))

;; Enabling this forces to deal with stdio.h for real, including typedefs.
;; This allows ftoc.c, K&R p. 12 to run.
(def ^:dynamic *allow-declarations?* false)
;; Contains the meta data with types etc. for current locals.
(def ^:dynamic *locals* {})

(defmethod compiler :declaration [[_ & [__extension__?
                                        declaration-specifiers
                                        initialized-declarator-list?]]]
  (when *allow-declarations?*
    (let [specifiers (compiler declaration-specifiers)]
      (map #(with-meta % specifiers)
           (remove nil? (compiler initialized-declarator-list?))))))

;; See http://jna.java.net/javadoc/overview-summary.html#marshalling for ideas how to map things.
;; http://nativelibs4java.sourceforge.net/bridj/api/development/org/bridj/Pointer.html
(defmethod compiler :int [_]
  {:tag Integer/TYPE
   :coercion `int})

(defmethod compiler :long [_]
  {:tag Long/TYPE
   :coercion `long})

(defmethod compiler :float [_]
  {:tag Float/TYPE
   :coercion `float})

(defmethod compiler :double [_]
  {:tag Double/TYPE
   :coercion `double})

;; Let's try byte, a char can be both signed/unsigned by default, "For gcc, the default is signed".
;; Also: Byte/toUnsignedInt
(defmethod compiler :char [_]
  {:tag Byte/TYPE
   :coercion `byte})

;; Doesn't take most of this stuff into account
(defmethod compiler :initialized-declarator [[_ & [attribute-specifier-list? declarator
                                                   simple-assembly-expression? attribute-specifier-list?
                                                   initializer?]]]
  (compiler declarator))

(defmethod compiler :initialized-declarator-list [[_ & declarators]]
  (map compiler declarators))

(defmethod compiler :declaration-specifiers [[_ & specifiers]]
  (apply merge (map compiler specifiers)))

(defmethod compiler :simple-declarator [[_ & [identifier]]]
  identifier)

(defmethod compiler :array-qualifier-list [[_ & qualifiers]]
  (map compiler qualifiers))

(defmethod compiler :array-declarator [[_ & [direct-declarator array-qualifier-list array-size-expression?]]]
  (println (compiler direct-declarator) (compiler array-qualifier-list) (compiler array-size-expression?)))

(defmethod compiler :primary-identifier [[_ & [identifier]]]
  identifier)

(defmethod compiler :integer-constant [[_ & [integer-literal]]]
  integer-literal)

(defmethod compiler :floating-constant [[_ & [float-literal]]]
  float-literal)

(defmethod compiler :string-constant [[_ & [string-literal]]]
  `(.getBytes ~string-literal))

(defmethod compiler :character-constant [[_ & [char-literal]]]
  (int char-literal))

;; Not a fan.
(defn deref? [x] (if (instance? IDeref x) @x x))

(defn maybe-deref [x] (if (symbol? x) (list `deref? x) x))

(def assignments {'+= `+ '-= `- '*= `* (symbol nil "/=") `/ '%= `mod
                  '<<= `bit-shift-left '>>= `bit-shift-right
                  '&= `bit-and (symbol "^=") `bit-xor '|= `bit-or})

(defmethod compiler :assignment-expression [[_ & [unary-expression assignment-operator assignment-expression]]]
  (let [variable (compiler unary-expression)
        coercion (:coercion (*locals* variable))]
    (if (= '= assignment-operator)
      `(reset! ~variable (~coercion ~(maybe-deref (compiler assignment-expression))))
      `(swap! ~variable (comp ~coercion ~(assignments assignment-operator))
              ~(maybe-deref (compiler assignment-expression))))))

;; This is premature, and '++ `inc '-- `dec '& `address needs special handling
(def unary-operators {'! `not (symbol "~") `bit-not})

(defn unary-operator [op x]
  `(~(unary-operator op (symbol "clojure.core" (str op))) ~(maybe-deref (compiler x))))

(defmethod compiler :subscript-expression [[_ & [postfix-expression expression]]]
  `(nth ~(compiler postfix-expression) ~(maybe-deref (compiler expression))))

(defmethod compiler :unary-minus-expression [[_ & [cast-expression]]]
  `(- ~(maybe-deref (compiler cast-expression))))

(defmethod compiler :preincrement-expression [[_ & [unary-expression]]]
  (let [variable (compiler unary-expression)
        coercion (:coercion (*locals* variable))]
    `(swap! ~variable (comp ~coercion inc))))

(def binary-operators {'== `= '!= `not= '% `mod '<< `bit-shift-left '>> `bit-shift-right
                       '& `bit-and (symbol "^") `bit-xor '| `bit-or})

(defn binary-operator [op x y]
  `(~(binary-operators op (symbol "clojure.core" (str op)))
    ~(maybe-deref (compiler x)) ~(maybe-deref (compiler y))))

;; Look into keyword inheritance for all this.
(defmethod compiler :relational-expression [[_ & [relational-expression relational-operator shift-expression]]]
  (binary-operator relational-operator relational-expression shift-expression))

(defmethod compiler :additive-expression [[_ & [additive-expression additive-operator multiplicative-expression]]]
  (binary-operator additive-operator additive-expression multiplicative-expression))

(defmethod compiler :multiplicative-expression [[_ & [multiplicative-expression multiplicative-operator cast-expression]]]
  (binary-operator multiplicative-operator multiplicative-expression cast-expression))

(defmethod compiler :equality-expression [[_ & [equality-expression equality-operator relational-expression]]]
  (binary-operator equality-operator equality-expression relational-expression))

(defmethod compiler :logical-or-expression [[_ & [logical-or-expression logical-and-expression]]]
  (binary-operator 'or logical-or-expression logical-and-expression))

(defmethod compiler :logical-and-expression [[_ & [logical-and-expression bitwise-or-expression]]]
  (binary-operator 'and logical-and-expression bitwise-or-expression))

(defmethod compiler :expression-list [[_ & expressions]]
  (map compiler expressions))

(defmethod compiler :function-call [[_ & [postfix-expression & [expression-list?]]]]
  `(~(compiler postfix-expression) ~@(map maybe-deref (compiler expression-list?))))

(defmethod compiler :if-statement [[_ & [expression statement]]]
  `(when ~(maybe-deref (compiler expression))
     ~(compiler statement)))

(defmethod compiler :if-else-statement [[_ & [expression statement else-statement]]]
  `(if ~(maybe-deref (compiler expression))
     ~(compiler statement)
     ~(compiler else-statement)))

(defmethod compiler :while-statement [[_ & [expression statement]]]
  `(while ~(maybe-deref (compiler expression))
     ~(compiler statement)))

(defmethod compiler :for-statement [[_ & [initial-clause expression expression? statement]]]
  `(do
     ~(compiler initial-clause)
     (while ~(maybe-deref (compiler expression))
       ~(compiler statement)
       ~(compiler expression?))))

(defmethod compiler :expression-statement [[_ & [expression]]]
  (compiler expression))

(defmethod compiler :compound-statement [[_ & args]]
  (let [[local-label-declaration*
         declaration-or-statement*] (split-with (comp #{:declaration} first)
                                                (butlast args))
         annotations (last args)
         local-labels (mapcat compiler local-label-declaration*)]
    (binding [*locals* (merge *locals* (zipmap local-labels (map meta local-labels)))]
      `(let ~(vec (mapcat #(vector % (list `atom nil)) local-labels))
         ~@(doall (map compiler declaration-or-statement*))))))

(defmethod compiler :function-declarator [[_ & [direct-declarator parameter-context]]]
  `(~(with-meta (compiler direct-declarator)
       (merge (meta direct-declarator)
              {:language :ansi-c})) [~@(compiler parameter-context)]))

(defmethod compiler :function-definition [[_ & [_ _ declarator _ compound-statement]]]
  (binding [*allow-declarations?* true] ;; Temporary hack.
    `(defn ~@(compiler declarator) ~(compiler compound-statement))))

(defn compile-and-link [file]
  (-> file
      preprocess-and-parse-c
      ast->clj
      compiler
      link))

(defn compile-and-run [file]
  (println "running" (str file))
  (compile-and-link file)
  (c/main))

(defn -main [& args]
  ;; K&R The C Programming Language examples.
  ;; We're cheating here, stubbing out the declarations and relying on the fact that printf is defined in Clojure.
  (doseq [f (filter #(re-find #"\.c$" (str %)) (.listFiles (io/file "resources/k&r")))]
    (with-in-str "hello world\nfrom\nstdin\n"
      (compile-and-run f))))
