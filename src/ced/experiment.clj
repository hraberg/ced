(ns ced.experiment
  (:require [clojure.java.io :as io]
            [clojure.core.reducers :as r]
            [clojure.string :as s]
            [clojure.zip :as z]
            [instaparse.core :as insta]
            [flatland.ordered.map :as om]
            [flatland.ordered.set :as os])
  (:import [java.util.regex Pattern]
           [java.util Map Set List]
           [clojure.lang Named ArityException]
           [flatland.ordered.set OrderedSet]))

(set! *warn-on-reflection* true)

;; Instaparse stuff

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
(arithmetic  "1+2+3")

;; Doesn't handle whitspace, the terminals are way oversimplified / not working. No proper enum/typedef during parse.
;; C EBNF from https://bitbucket.org/jlyonsmith/ebnf-visualizer/raw/1a866feaf38d34599027b37e362cfe42b9b6ba31/Grammars/c.ebnf
(def c-parser (insta/parser (str (io/resource "c.ebnf"))))
(c-parser "main(){}")

;; Experimental parser, this isn't built on some nice theoretical basis.
;; Just playing with a rough idea I've had nagging me around combining all phases in one go.
;; I would like Ced to have a pre-processor and C-to-Clojure compiler in one walk from source without external dependencies.
;; Not because it's practical, but to see if something interesting falls out from constraint.

;; Can/should this guy be folded into Mímir somehow? (Assuming it starts working properly.)

(defn maybe-singleton
  ([])
  ([x] x)
  ([x & args] (vec (cons x args))))

(defn suppressed-rule? [r]
  (when-let [[ _ r] (re-find #"^<(.+)>$" (name r))]
    (keyword r)))

(def ^:dynamic *allow-split-tokens* true)
(def ^:dynamic *memoize-tokenization* false)
(def ^:dynamic *pre-delimiter* #"\s*")
(def ^:dynamic *post-delimiter* (if *allow-split-tokens* #"" #"(:?\s+|$)"))
(def ^:dynamic *offset* 0)
(def ^:dynamic *rule* nil)
(def ^:dynamic *default-result* [])
(def ^:dynamic *token-fn* conj)
(def ^:dynamic *suppress-tags* false)
(def ^:dynamic *node-fn* (fn [& args]
                           (if (or *suppress-tags* (suppressed-rule? *rule*))
                             (apply maybe-singleton args)
                             [*rule* (apply maybe-singleton args)])))
(def ^:dynamic *default-action* maybe-singleton)
(def ^:dynamic *grammar-actions* true)
(def ^:dynamic *alternatives-rank* (comp count flatten :result))
(def ^:dynamic *grammar* {})
(def ^:dynamic *failure-grammar* {:no-match [#"\S*" #(throw (IllegalStateException. (str "Don't know how to parse: " %)))]})
(def ^:dynamic *start-rule* first)
(def ^:dynamic *extract-result* (comp first :result))
(def ^:dynamic *rules-seen-at-point* #{})

(defn ctor-for-named [n]
  (resolve (symbol (s/lower-case (.getSimpleName ^Class (type n))))))

(defn suppressed-defintion? [r]
  (let [suppressed-defintion ((ctor-for-named r) (str "<" (name r) ">"))]
    (if (*grammar* suppressed-defintion)
      suppressed-defintion
      r)))

(defrecord StringParser [string offset token result])

(defn string-parser
  ([s] (if (instance? StringParser s) s (string-parser s *default-result*)))
  ([s result] (StringParser. s 0 nil result)))

(defn at-end? [{:keys [string offset] :as in}]
  (= offset (count string)))

(defn try-parse [{:keys [string offset result] :as in} ^Pattern re]
  (when in
    (let [m (re-matcher re (subs string offset))]
      (when (.lookingAt m)
        (assoc in
          :offset (+ offset (.end m 0))
          :token (.group m 0))))))

(defn try-parse-skip-delimiter [in m]
  (when-let [{:keys [token] :as in} (if-let [result (try-parse in m)]
                                      result
                                      (-> in
                                          (try-parse *pre-delimiter*)
                                          (try-parse m)))]
    (when-let [in (try-parse in *post-delimiter*)]
      (assoc in :token token))))

(defn next-token [in m capture?]
  (when-let [{:keys [token offset] :as in} (try-parse-skip-delimiter in m)]
    (assoc (if capture?
             (binding [*offset* offset]
               (->  in
                    (update-in [:result] *token-fn* token)))
             in) :token nil)))

;; We're actually rather want something like this, which acts on a higher level:
;; http://cs.nyu.edu/rgrimm/xtc/rats-intro.html#transient
(when *memoize-tokenization*
  (alter-var-root #'next-token memoize))

(defn name-and-predicate [n]
  (let [ctor (ctor-for-named n)
        [_ predicate n] (re-find #"^([!&]?)(.+)" (name n))]
    [(ctor n) (when (seq predicate) (symbol predicate))]))

(defn name-and-quantifier [n]
  (let [ctor (ctor-for-named n)
        [_ n quantifier] (re-find #"(.+?)([+*?]?)$" (name n))]
    [(ctor n) (when (seq quantifier) (symbol quantifier))]))

(defn fold-into [ctor coll]
  (r/fold (r/monoid into ctor) conj coll))

;; This could potentially be a tree, but requires to restructure and use reducers all over the place.
(defn valid-choices [in ms]
  (fold-into vector (r/remove nil? (r/map #(parse % in) (vec ms)))))

;; Not sure this name is right
(defprotocol IParser
  (parse [this] [this in]))

(extend-protocol IParser
  Pattern
  (parse [this in]
    (next-token in this true))

  Character
  (parse [this in]
    (parse (str this) in))

  String
  (parse
    ([this] (parse (string-parser this)))
    ([this in]
       (next-token in (re-pattern (Pattern/quote this)) false)))

  Named
  (parse [this in]
    (when-not (*rules-seen-at-point* [this in])  ;; Only guards against StackOverflow, doesn't actually handle left recursion.
      (binding [*rules-seen-at-point* (conj *rules-seen-at-point* [this in])]
        (let [[this quantifier] (name-and-quantifier this)
              [this predicate] (name-and-predicate this)
              suppressed (suppressed-rule? this)
              this (suppressed-defintion? this)]
          (if-let [[rule action] (some *grammar* [this suppressed])]
            (letfn [(parse-one [in]
                      (let [current-result (:result in)]
                        (when-let [result (parse rule (assoc in :result *default-result*))]
                          (binding [*rule* this]
                            (update-in result [:result]
                                       #(*token-fn* current-result
                                                    (*node-fn* (try
                                                                 (apply (or (when *grammar-actions* action)
                                                                            *default-action*) %)
                                                                 (catch ArityException _
                                                                   (apply *default-action* %))))))))))
                    (parse-many [in quantifier]
                      (case quantifier
                        ? (or (parse-one in) in)
                        * (loop [in in]
                            (if-let [in (parse-one in)]
                              (recur in)
                              in))
                        + (when-let [in (parse-one in)]
                            (parse-many in '*))
                        (parse-one in)))]
              (let [result (parse-many in quantifier)]
                (case predicate
                  ! (when-not result in)
                  & (when result in)
                  result)))
            (throw (IllegalStateException. (str "Unknown rule: " this))))))))

  Set
  (parse [this in]
    (when-let [alternatives (seq (distinct (valid-choices in this)))]
      (apply max-key :offset (sort-by *alternatives-rank* alternatives))))

  OrderedSet
  (parse [this in]
    (first (valid-choices in this)))

  Map
  (parse [this in]
    (binding [*grammar* this]
      (parse (*start-rule* (os/into-ordered-set (keys this))) (string-parser in))))

  List
  (parse [this in]
    (loop [in in
           [m & m-rst] this]
      (if (and in m (not (at-end? in)))
        (recur (parse m in) m-rst)
        (when-not m in))))

  StringParser
  (parse
    ([this] (parse *grammar* this))
    ([this parser]
       (parse parser this))))

(def choice os/ordered-set)

(defn fun [s]
  (resolve (symbol s)))

(defn op
  ([op x] ((fun op) x))
  ([x op y] ((fun op) x y)))

(defn rule? [r]
  (and (vector? r) (= 2 (count r)) (fn? (last r))))

(defn grammar [& rules]
  (let [rules (mapcat (fn [[rs [f]]] (if f (conj (vec (butlast rs)) [(last rs) f]) rs))
                      (partition-all 2 (partition-by fn? rules)))]
    (into (om/ordered-map) (map (fn [[name rule]] [name (if (rule? rule)
                                                          rule
                                                          [rule])])
                                (partition 2 rules)))))

(defn parser-options [options]
  (into {} (map (fn [[k v]]
                  [(if (keyword? k)
                     (or (resolve (symbol (str "*" (name k) "*")))
                         (throw (IllegalArgumentException. (str "Unknown option: " k))))
                     k) v]) options)))

;; Starts getting clunky, holding off to macrofiy it as this is not the core issue.
(defn create-parser
  ([& rules]
     (let [[[default-options] rules] (split-with map? rules)
           grammar (apply grammar rules)]
       (fn parser
         ([in & options]
            (with-bindings (merge (parser-options default-options) (parser-options (apply hash-map options)))
              (when-let [in (parse grammar in)]
                (if (at-end? in)
                  (*extract-result* in)
                  (parse *failure-grammar* in)))))))))

(def expression (create-parser
                 :expr      :add-sub
                 :<add-sub> #{:mul-div :add :sub}
                 :add       [:add-sub "+" :mul-div]
                 :sub       [:add-sub "-" :mul-div]
                 :<mul-div> #{:term :mul :div}
                 :mul       [:mul-div "*" :term]
                 :div       [:mul-div "/" :term]
                 :<term>    #{:number ["(" :add-sub ")"]}
                 :number    #"[0-9]+"))

(expression "1")

;; Stopped working after starting from first rule only, now needs left recurision.
;(expression "1/2")
;(expression "2+5*2")
;; Doesn't work yet
;(expression "1+2+3")
;; Need to handle left recursion, tree from instaparse:
;; [:expr [:add [:add [:number "1"] [:number "2"]] [:number "3"]]]

;;(expression "1-2/(3-4)+5*6")

;; PEG example from http://bford.info/pub/lang/packrat-icfp02-slides.pdf
;; Additive → Multitive '+' Additive
;; | Multitive
;; Multitive → Primary '*' Multitive
;; | Primary
;; Primary → '(' Additive ')'
;; | Decimal
;; Decimal → '0' | ... | '9'

;; Should arguebly use choice instead of #{}
(def peg-expression (create-parser
                     {:suppress-tags true}

                     :additive  #{[:multitive #"[+-]" :additive]
                                  :multitive} op
                     :multitive #{[:primary #"[*/]" :multitive]
                                  :primary} op
                     :primary   #{["(" :additive ")"]
                                  :decimal}
                     :decimal   #"[0-9]+" read-string))

;; This gets wrong precedence, regardless of using choice / OrderedSet or not. So something else.
(peg-expression "1-2/(3-4)+5*6")

(peg-expression "2+5*2")
(peg-expression "2+5*2" :grammar-actions false :suppress-tags false)

;; A different expression grammar from:
;; http://www.cs.umd.edu/class/fall2002/cmsc430/lec4.pdf

;; Left recursive
;; 1 <goal> ::= <expr>
;; 2 <expr> ::= <expr> + <term>
;; 3 | <expr> - <term>
;; 4 | <term>
;; 5 <term> ::= <term> * <factor>
;; 6 | <term> = <factor>
;; 7 | <factor>
;; 8 <factor> ::= number
;; 9 | id

;; Right recursive
;; 1 <goal> ::= <expr>
;; 2 <expr> ::= <term> + <expr>
;; 3 | <term> - <expr>
;; 4 | <term>
;; 5 <term> ::= <factor> * <term>
;; 6 | <factor> / <term>
;; 7 | <factor>
;; 8 <factor> ::= number
;; 9 | id
