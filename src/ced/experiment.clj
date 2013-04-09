(ns ced.experiment
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta])
  (:import [java.util.regex Pattern]))

(set! *warn-on-reflection* true)

(def arithmetic
  (insta/parser
    "expr = add-sub
     <add-sub> = mul-div | add | sub
     add = add-sub <'+'> mul-div
     sub = add-sub <'-'> mul-div
     <mul-div> = term | mul | div
     mul = mul-div <'*'> term
     div = mul-div <'/'> term
     <term> = number | <'('> add-sub <')'>
     number = #'[0-9]+'"))

(->> (arithmetic "1-2/(3-4)+5*6")
     (insta/transform
       {:add +, :sub -, :mul *, :div /,
        :number read-string :expr identity}))

(->> (arithmetic "2+5*2")
     (insta/transform
       {:add +, :sub -, :mul *, :div /,
        :number read-string :expr identity}))

;; Doesn't handle whitspace, the terminals are way oversimplified / not working. No proper enum/typedef during parse.
;; C EBNF from https://bitbucket.org/jlyonsmith/ebnf-visualizer/raw/1a866feaf38d34599027b37e362cfe42b9b6ba31/Grammars/c.ebnf
(def c-parser (insta/parser (slurp (io/resource "c.ebnf"))))
(c-parser "main(){}")

;; Experimental parser, this isn't built on some nice theoretical basis.
;; Just playing with a rough idea I've had nagging me around combining all phases in one go.
;; I would like Ced to have a pre-processor and C-to-Clojure compiler in one walk from source without external dependencies.
;; Not because it's practical, but to see if something interesting falls out from constraint.

(def ^:dynamic *s*)
(def ^:dynamic *delimiter* #"\s+")

(defn try-parse [^Pattern re]
  (let [{:keys [string offset]} @*s*
        m (re-matcher re (subs string offset))]
    (when (.lookingAt m)
      (swap! *s* update-in [:offset] + (.end m 0))
      (.group m 0))))

(defn grammar [& rules]
  (vec (map vec (partition 2 (apply list rules)))))

(def fun (comp resolve symbol))
(defn binary-op [x op y] ((fun op) x y))

;; Doesn't work properly, like operator precedance among other things.
(def ^:dynamic *grammar*
  (grammar
   :add [[:add-sub "+" :mul-div] binary-op]
   :sub [[:add-sub "-" :mul-div] binary-op]
   :mul [[:mul-div "*" :term] binary-op]
   :div [[:mul-div "/" :term] binary-op]
   :add-sub [[[:mul-div :add :sub]]]
   :mul-div [[[:term :mul :div]]]
   :term [[#{:number ["(" :add-sub ")"]}]]
   :number [#"[0-9]+" read-string]))

(declare parse-all)

(defn parse-re [m f]
  (when-let [token (do
                     (try-parse *delimiter*)
                     (try-parse m))]
    (f token)))

(defn parse-any [grammar]
  (some (fn [[n p]]
          (if-let [token (some #(when (-> % meta :token-types set n) %)
                               (:unused-tokens @*s*))] ;; 5 minutes later I have no clue how this actually works.
            (do
              (swap! *s* assoc-in [:unused-tokens] [])
              token)
            (let [[m f] (if (sequential? p) p [p])
                  f (if f f identity)
                  result (cond
                          (instance? Pattern m) [n (parse-re m f)]
                          (string? m) [n (parse-re (re-pattern (Pattern/quote m)) f)]
                          (vector? m) (parse-all n m f))]
              (when (second result)
                (swap! *s* assoc-in [:unused-tokens] [result])
                result))))
        (conj grammar
              [:no-match [#"\S*" #(throw (IllegalStateException. (str "Don't know how to parse: " %)))]])))

(defn terminal? [x]
  (and (vector? x) (keyword? (first x))))

(def ^:dynamic *seen-rules* #{})

(defn parse-all [n rules f]
  (if (*seen-rules* rules)
    (throw (IllegalStateException. "Infinite Loop"))
    (binding [*seen-rules* (conj *seen-rules* rules)]
      (let [before @*s*
            known-rule? (into {} *grammar*)
            map-rules (fn map-rules [rd]
                        (if (coll? rd)
                          (vec (mapcat map-rules rd))
                          (if-let [r (known-rule? rd)]
                            [[rd r]]
                            [[(keyword (gensym)) rd]])))]
        (try
          (try
            (let [result (map parse-any (map map-rules rules))]
              (with-meta [n (apply f (map second result))]
                {:token-types (vec (map first result))}))
            (finally
             (swap! *s* assoc-in [:unused-tokens] [])))
          (catch IllegalStateException _
            (do (reset! *s* before)
                nil)))))))

(defn parse
  ([s grammar] (binding [*s* (atom {:string s :offset 0 :unused-tokens []})
                         *grammar* grammar]
                 (doall (for [_ (take-while true? (repeatedly #(> (count (:string @*s*))
                                                                  (:offset @*s*))))]
                          (parse-any *grammar*)))))
  ([s] (parse s *grammar*)))
