(ns ced.main
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.java.shell :as sh]
            [clojure.string :as s]
            [clojure.walk :as w])
  (:import [java.io ByteArrayOutputStream PrintStream StringReader File]
           [clojure.lang Reflector IDeref Ratio]
           [org.anarres.cpp Preprocessor PreprocessorListener CppReader
            Feature Warning LexerSource StringLexerSource FileLexerSource]
           [xtc.lang C]
           [xtc.parser ParseError]
           [xtc.tree Node])
  (:gen-class))

(def ed "ed-1.7")
(def musl "musl-0.9.9")

(set! *warn-on-reflection* true)

(defn ^LexerSource lexer-source [file]
  (if (and (not (.exists (io/file file))) (string? file))
    (StringLexerSource. file true)
    (FileLexerSource. (io/file file))))

;; We want to get rid of JCPP if possible and bake the preprocessor into the main parse.
;; SuperC in xtc should be able to do this, but creates a huge, seemingly wrong, AST.
(defn cpp [file]
  (let [pp (doto (Preprocessor.)
             (.addFeature Feature/DIGRAPHS);
             (.addFeature Feature/TRIGRAPHS);
             (.addWarning Warning/IMPORT);
             (.setListener (PreprocessorListener.))
             (.addMacro "__JCPP__")
             (.setSystemIncludePath [(str musl "/include")])
             (.addInput (lexer-source file)))]
    (slurp (CppReader. pp))))

(defn cpp-gcc [file]
  (:out (sh/sh "cpp" "-E" "-P" file)))

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

(defn ast->clj [node]
  (cond
   (instance? Node node) (with-meta
                           (cons
                            (keyword
                             (s/replace (decamel (.getName ^Node node)) #"^xtc.tree." ""))
                            (map ast->clj (seq node)))
                           {:location  (location node)
                            :node node})
   (string? node) (read-string node)
   :else node))

;; Using Rats! C parser, decoupled from the xtc source tree (see src/xtc).
;; We could look into transforming the grammar, by hand or programatically, into some Clojure PEG parser.
(defn parse-c [source file]
  (let [parser (C. (StringReader. source) file)
        tu (.pTranslationUnit parser 0)]
    (if-not (.hasValue tu)
      (.signal parser (.parseError tu))
      (.semanticValue tu))))

(defn preprocess-and-parse-c [file]
  (let [file (io/file file)]
    (parse-c (cpp file) (.getName file))))

(defn parse-ed []
  (time
   (doseq [^File f (filter (fn [^File f] (re-find #"\.c$" (.getName f))) (.listFiles (io/file ed)))
           :let [ns (symbol (str "ced.ed." (s/replace (.getName f) #"\.c$" "")))]]
     (create-ns ns)
     (println "preprocessing and parsing" (str f))
     (time (let [pp (cpp (.getAbsolutePath f))]
             (intern ns 'c-source (ast->clj (parse-c pp (.getName f)))))))))

(defmulti compiler (fn [[type & args]] type))

(defmethod compiler nil [[_ & args]])

(defmethod compiler :default [[_ & args]]
  (println "don't know how to compile" _))

(binding [*ns* (create-ns 'c)]
  (clojure.core/refer-clojure)
  (intern *ns* 'main))

(defn link [object]
  (binding [*ns* (the-ns 'c)]
    (eval object)))

(defmethod compiler :translation-unit [[_ & args]]
  (let [prelude? (first args)
        external-declaration* (next (butlast args))
        annotations (last args)
        tu (cons (compiler prelude?) (map compiler external-declaration*))]
    (doall (cons 'do (remove nil? tu)))))

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

(defmethod compiler :int [_]
  {:tag Integer/TYPE
   :coercion `int})

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

(defmethod compiler :primary-identifier [[_ & [identifier]]]
  identifier)

(defmethod compiler :integer-constant [[_ & [integer-literal]]]
  integer-literal)

(defmethod compiler :string-constant [[_ & [string-literal]]]
  string-literal)

;; Not a fan.
(defn ? [x] (if (instance? IDeref x) @x x))

(defn maybe-deref [x] (if (symbol? x) (list `? x) x))

(def assignments {'+= `+ '-= `- '*= `* (symbol nil "/=") `/ '%= `mod
                  '<<= `bit-shift-left '>>= `bit-shift-right
                  '&= `bit-and (symbol nil "^=") `bit-xor '|= `bit-or})

(defmethod compiler :assignment-expression [[_ & [unary-expression assignment-operator assignment-expression]]]
  (let [variable (compiler unary-expression)
        coercion (:coercion (*locals* variable))]
    (if (= '= assignment-operator)
      (list 'reset! variable
            (list coercion (maybe-deref (compiler assignment-expression))))
      (list 'swap! variable
            (list `comp
                  coercion
                  (assignments assignment-operator))
            (maybe-deref (compiler assignment-expression))))))

(defmethod compiler :relational-expression [[_ & [releational-expression relational-operator shift-expression]]]
  (cons relational-operator (map maybe-deref [(compiler releational-expression) (compiler shift-expression)])))

(defmethod compiler :additive-expression [[_ & [additive-expression additive-operator multiplicative-expression]]]
  (cons additive-operator (map maybe-deref [(compiler additive-expression) (compiler multiplicative-expression)])))

(defmethod compiler :multiplicative-expression [[_ & [multiplicative-expression multiplicative-operator cast-expression]]]
  (cons multiplicative-operator (map maybe-deref [(compiler multiplicative-expression) (compiler cast-expression)])))

(defmethod compiler :expression-list [[_ & expressions]]
  (map compiler expressions))

(defmethod compiler :function-call [[_ & [postfix-expression & [expression-list?]]]]
  (cons (compiler postfix-expression)
        (map maybe-deref (compiler expression-list?))))

(defmethod compiler :while-statement [[_ & [expression statement]]]
  (list 'while (compiler expression)
        (compiler statement)))

(defmethod compiler :expression-statement [[_ & [expression]]]
  (compiler expression))

(defmethod compiler :compound-statement [[_ & args]]
  (let [[local-label-declaration*
         declaration-or-statement*] (split-with (comp #{:declaration} first)
                                                (butlast args))
         annotations (last args)
         local-labels (mapcat compiler local-label-declaration*)]
    (binding [*locals* (merge *locals* (zipmap local-labels (map meta local-labels)))]
      (doall (concat ['let (vec (mapcat #(vector % (list 'atom nil)) local-labels))]
                     (map compiler declaration-or-statement*))))))

(defmethod compiler :function-declarator [[_ & [direct-declarator parameter-context]]]
  (list (with-meta (compiler direct-declarator) (merge (meta direct-declarator)
                                                        {:language :ansi-c}))
        (vec (compiler parameter-context))))

(defmethod compiler :function-definition [[_ & [_ _ declarator _ compound-statement]]]
  (binding [*allow-declarations?* true] ;; Temporary hack.
    (doall (cons 'defn (concat (compiler declarator) [(compiler compound-statement)])))))

(defn compile-and-link [file]
  (-> file
      preprocess-and-parse-c
      ast->clj
      compiler
      link))

(defn compile-and-run [file]
  (println "running" file)
  (compile-and-link file)
  (c/main))

(defn -main [& args]
  ;; We'll get back to this, acts as a smoke test.
  (parse-ed)
  ;; We're cheating here, stubbing out the declarations and relying on the fact that printf is defined in Clojure.
  (compile-and-run "resources/hello.c") ;; K&R, p. 10.
  (compile-and-run "resources/ftoc.c")) ;; K&R, p. 12.
