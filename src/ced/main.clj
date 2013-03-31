(ns ced.main
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as s]
            [clojure.walk :as w])
  (:import [java.io ByteArrayOutputStream PrintStream StringReader File]
           [clojure.lang Reflector]
           [org.anarres.cpp Preprocessor PreprocessorListener
            Feature Warning StringLexerSource FileLexerSource Token]
           [xtc.lang C]
           [xtc.parser ParseError]
           [xtc.tree Node])
  (:gen-class))

(def ed "ed-1.7")
(def musl "musl-0.9.9")

(set! *warn-on-reflection* true)

(defmacro with-java-out-str [& body]
  `(let [o# System/out
         bos# (ByteArrayOutputStream.)]
     (try (System/setOut (PrintStream. bos#))
          ~@body
          (String. (.toByteArray bos#) "UTF-8")
          (finally (System/setOut o#)))))

(defn gcc-include-path []
  (let [lib-gcc (io/file "/usr/lib/gcc/")]
    (when (.exists lib-gcc)
      (some (fn [^File f] (when (= "include" (.getName f)) (.getAbsolutePath f)))
            (file-seq lib-gcc)))))

(defn cpp [file]
  (let [pp (doto (Preprocessor.)
             (.addFeature Feature/DIGRAPHS);
             (.addFeature Feature/TRIGRAPHS);
             (.addWarning Warning/IMPORT);
             (.setListener (PreprocessorListener.))
             (.addMacro "__JCPP__")
             (.addInput (FileLexerSource. (io/file file))))]
    (doto (.getSystemIncludePath pp)
      (.add (str musl "/include")))
    (loop [tok (.token pp) s (StringBuilder.)]
      (if (and tok (not= Token/EOF (.getType tok)))
        (recur (.token pp) (.append s (.getText tok)))
        (str s)))))

(defn cpp-gcc [file]
  (:out (sh/sh "cpp" "-E" "-P" file)))

(defn print-ast [node]
  (import xtc.tree.Printer)
  (Reflector/invokeInstanceMethod
   (eval `(Printer. *out*))
   "format"
   (object-array [(if (instance? Node node)
                    node
                    (:node (meta node)))]))
  node)

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
                             (decamel (.getName ^Node node)))
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

(defn parse-ed []
  (time
   (doseq [^File f (filter (fn [^File f] (re-find #"\.c$" (.getName f))) (.listFiles (io/file ed)))
           :let [ns (symbol (str "ced.ed." (s/replace (.getName f) #"\.c$" "")))]]
     (create-ns ns)
     (println "preprocessing and parsing" (str f))
     (time (let [pp (cpp (.getAbsolutePath f))]
             (intern ns 'c-source (ast->clj (parse-c pp (.getName f)))))))))

(defn -main [& args]
  (parse-ed))
