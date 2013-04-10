(ns ced.experiment
  (:require [clojure.java.io :as io]
            [clojure.zip :as z]
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

(defn evaluate [ast]
  (insta/transform {:add +, :sub -, :mul *, :div /,
                    :number read-string :expr identity}
                   ast))

(evaluate (arithmetic "1-2/(3-4)+5*6"))

(evaluate (arithmetic "2+5*2"))

;; Doesn't handle whitspace, the terminals are way oversimplified / not working. No proper enum/typedef during parse.
;; C EBNF from https://bitbucket.org/jlyonsmith/ebnf-visualizer/raw/1a866feaf38d34599027b37e362cfe42b9b6ba31/Grammars/c.ebnf
(def c-parser (insta/parser (str (io/resource "c.ebnf"))))
(c-parser "main(){}")

;; Experimental parser, this isn't built on some nice theoretical basis.
;; Just playing with a rough idea I've had nagging me around combining all phases in one go.
;; I would like Ced to have a pre-processor and C-to-Clojure compiler in one walk from source without external dependencies.
;; Not because it's practical, but to see if something interesting falls out from constraint.

(def ^:dynamic *delimiter* #"\s*")
(def ^:dynamic *offset* 0)
(def ^:dynamic *default-result* [])
(def ^:dynamic *default-result-fn* conj)

(defn in-str
  ([s] (in-str s *default-result*))
  ([s result]
     {:string s :offset 0 :token nil :result result}))

(defn at-end? [{:keys [string offset] :as in}]
  (= offset (count string)))

(defn try-parse [{:keys [string offset result] :as in} ^Pattern re]
  (when in
    (let [m (re-matcher re (subs string offset))]
      (when (.lookingAt m)
        {:string string
         :offset (+ offset (.end m 0))
         :token (.group m 0)
         :result result}))))

(defn try-parse-skip-delimiter [in m]
  (if-let [result (try-parse in m)]
    result
    (-> in
        (try-parse *delimiter*)
        (try-parse m))))

(defn grammar [& rules]
  (vec (map vec (partition 2 (apply list rules)))))

(def fun (comp resolve symbol))
(defn binary-op [x op y] ((fun op) x y))

;; Doesn't work properly, like operator precedance among other things.
(def ^:dynamic *grammar*
  "expr = add-sub
     <add-sub> = mul-div | add | sub
     add = add-sub <'+'> mul-div
     sub = add-sub <'-'> mul-div
     <mul-div> = term | mul | div
     mul = mul-div <'*'> term
     div = mul-div <'/'> term
     <term> = number | <'('> add-sub <')'>
     number = #'[0-9]+'"
   ;;  (grammar
   ;; :add [[:add-sub "+" :mul-div] binary-op]
   ;; :sub [[:add-sub "-" :mul-div] binary-op]
   ;; :mul [[:mul-div "*" :term] binary-op]
   ;; :div [[:mul-div "/" :term] binary-op]
   ;; :add-sub [[[:mul-div :add :sub]]]
   ;; :mul-div [[[:term :mul :div]]]
   ;; :term [[[:number ["(" :add-sub ")"]]]]
   ;; :number [#"[0-9]+" read-string])
  (grammar
   :add [[:add-sub "+" :mul-div]]
   :sub [[:add-sub "-" :mul-div]]
   :mul [[:mul-div "*" :term]]
   :div [[:mul-div "/" :term]]
   :add-sub [[[:mul-div :add :sub]]]
   :mul-div [[[:term :mul :div]]]
   :term [[[:number ["(" :add-sub ")"]]]]
   :number [#"[0-9]+"]))

(declare parse-all)

(defn parse-re
  ([in m] (parse-re in m *default-result-fn*))
  ([in m f]
     (when-let [{:keys [token offset] :as in} (try-parse-skip-delimiter in m)]
       (binding [*offset* offset]
         (update-in in [:result] f token)))))

(defn parse-alts
  ([in alts] (parse-alts in alts *default-result-fn*))
  ([in alts f]
     (apply max-key :offset
            (map #(parse-re in % f) alts))))

(defn ensure-seq [x]
  (if (sequential? x) x (repeat x)))

(defn parse-re-seq
  ([in m] (parse-re-seq in m *default-result-fn*))
  ([in m f]
     (loop [in in
            [m & m-rst] (ensure-seq m)]
       (if (and in m (not (at-end? in)))
         (recur (parse-re in m f) m-rst)
         in))))

;; Ancient crap from yesterday.

;; (defn parse-any [grammar]
;;   (some (fn [[n p]]
;;           (if-let [token (some #(when (-> % meta :token-types set n) %)
;;                                (:unused-tokens @*s*))] ;; 5 minutes later I have no clue how this actually works.
;;             (do
;;               (swap! *s* assoc-in [:unused-tokens] [])
;;               token)
;;             (let [[m f] (if (sequential? p) p [p])
;;                   f (if f f (fn [& args] (if (= 1 (count args)) (first args) args)))
;;                   result (cond
;;                           (instance? Pattern m) [n (parse-re m f)]
;;                           (string? m) [n (parse-re (re-pattern (Pattern/quote m)) f)]
;;                           (vector? m) (parse-all n m f))]
;;               (when (second result)
;;                 (swap! *s* assoc-in [:unused-tokens] [result])
;;                 result))))
;;         (conj grammar
;;               [:no-match [#"\S*" #(throw (IllegalStateException. (str "Don't know how to parse: " %)))]])))

;; (defn terminal? [x]
;;   (and (vector? x) (keyword? (first x))))

;; (def ^:dynamic *seen-rules* #{})

;; (defn parse-all [n rules f]
;;   (if (*seen-rules* rules)
;;     (throw (IllegalStateException. "Infinite Loop"))
;;     (binding [*seen-rules* (conj *seen-rules* rules)]
;;       (let [before @*s*
;;             known-rule? (into {} *grammar*)
;;             map-rules (fn map-rules [rd]
;;                         (if (coll? rd)
;;                           (vec (mapcat map-rules rd))
;;                           (if-let [r (known-rule? rd)]
;;                             [[rd r]]
;;                             [[(keyword (gensym)) rd]])))]
;;         (try
;;           (try
;;             (let [result (map parse-any (map map-rules rules))]
;;               (with-meta [n (apply f result ;(map second result)
;;                                    )]
;;                 {:token-types (vec (map first result))}
;;                 ))
;;             (finally
;;              (swap! *s* assoc-in [:unused-tokens] [])))
;;           (catch IllegalStateException _
;;             (do (reset! *s* before)
;;                 nil)))))))

;; (defn parse
;;   ([s grammar] (binding [*s* (atom {:string s :offset 0 :unused-tokens []})
;;                          *grammar* grammar]
;;                  (doall (for [_ (take-while true? (repeatedly #(> (count (:string @*s*))
;;                                                                   (:offset @*s*))))]
;;                           (parse-any *grammar*)))))
;;   ([s] (parse s *grammar*)))
