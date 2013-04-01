(ns ced.main
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.java.shell :as sh]
            [clojure.string :as s]
            [clojure.walk :as w])
  (:import [java.io ByteArrayOutputStream PrintStream StringReader File]
           [clojure.lang Reflector]
           [org.anarres.cpp Preprocessor PreprocessorListener
            Feature Warning LexerSource StringLexerSource FileLexerSource Token]
           [xtc.lang C]
           [xtc.parser ParseError]
           [xtc.tree Node])
  (:gen-class))

(def ed "ed-1.7")
(def musl "musl-0.9.9")

(set! *warn-on-reflection* true)

(defn cpp [file]
  (let [^LexerSource source (if (and (not (.exists (io/file file))) (string? file))
                              (StringLexerSource. file true)
                              (FileLexerSource. (io/file file)))
        pp (doto (Preprocessor.)
             (.addFeature Feature/DIGRAPHS);
             (.addFeature Feature/TRIGRAPHS);
             (.addWarning Warning/IMPORT);
             (.setListener (PreprocessorListener.))
             (.addMacro "__JCPP__")
             (.addInput source))]
    (doto (.getSystemIncludePath pp)
      (.add (str musl "/include")))
    (loop [tok (.token pp) s (StringBuilder.)]
      (if (and tok (not= Token/EOF (.getType tok)))
        (recur (.token pp) (.append s (.getText tok)))
        (str s)))))

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
  (clojure.core/refer-clojure))

(defmethod compiler :translation-unit [[_ & args]]
  (let [prelude? (first args)
        external-declaration* (drop 1 (butlast args))
        annotations (last args)
        tu (cons (compiler prelude?) (map compiler external-declaration*))]
    (binding [*ns* (the-ns 'c)]
      (doall (map eval tu)))))

;; #define - stubbed out for now.
(defmethod compiler :declaration [[_ & [__extension__?
                                        declaration-specifiers
                                        initialized-declarator-list?]]])

(defmethod compiler :simple-declarator [[_ & [identifier]]]
  identifier)

(defmethod compiler :primary-identifier [[_ & [identifier]]]
  identifier)

(defmethod compiler :string-constant [[_ & [string-literal]]]
  string-literal)

(defmethod compiler :expression-list [[_ & expressions]]
  (map compiler expressions))

(defmethod compiler :function-call [[_ & [postfix-expression & [expression-list?]]]]
  (cons (compiler postfix-expression)
        (compiler expression-list?)))

(defmethod compiler :expression-statement [[_ & [expression]]]
  (compiler expression))

(defmethod compiler :compound-statement [[_ & args]]
  (let [local-label-declaration*-declaration-or-statement* (butlast args)
        annotations (last args)]
    (cons 'do
          (map compiler local-label-declaration*-declaration-or-statement*))))

(defmethod compiler :function-declarator [[_ & [direct-declarator parameter-context]]]
  (list (with-meta (compiler direct-declarator) (merge (meta direct-declarator)
                                                        {:language :ansi-c}))
        (vec (compiler parameter-context))))

(defmethod compiler :function-definition [[_ & [_ _ declarator _ compound-statement]]]
  (cons 'defn (concat (compiler declarator) [(compiler compound-statement)])))

(defn -main [& args]
  ;; We'll get back to this, acts as a smoke test.
  (parse-ed)
  ;; We're cheating here, stubbing out the declarations and relying on the fact that printf is defined in Clojure.
  (-> "resources/hello.c" ;; K&R, p. 10.
      preprocess-and-parse-c
      ast->clj
      compiler)
  (eval '(c/main)))
