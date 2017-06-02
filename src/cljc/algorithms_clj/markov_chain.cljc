(ns cljc.algorithms-clj.markov-chain
  (:require
    [clojure.string :as str]
    [clojure.test :as test :refer [deftest is are testing]]
    ))


;; Different ways to extract the information from text

(defn split-words
  [text]
  (str/split text #" "))

#_(defn split-class-name
    [class-name]
    (map (fn [[a b]]
           (str (apply str a) (apply str b)))
      (partition 2
        (partition-by #(Character/isUpperCase %) class-name))))

(defn map-values
  [f m]
  (persistent!
    (reduce-kv
      (fn [res k v] (assoc! res k (f v)))
      (transient {})
      m)))


;; Weighted choice
;; TODO - use a better algorithm... this one is linear
;; TODO - make a preparation phase for this algo as well... this is not good

(defn weighted-keys->gen
  "Given a map of generators and weights, return a value from one of
   the generators, selecting generator based on weights."
  [m]
  (let [weights (reductions + (vals m))
        total (last weights)
        choices (map vector (keys m) weights)]
    (fn []
      (let [choice (rand-int total)]
        (loop [[[val w] & more] choices]
          (if (< choice w) val (recur more)))))))

(defn run-gen [gen] (gen))


;; Construct the markov-chain
;; TODO - have a specific transducer for this
;; TODO - Allow to feed the transitions several times (for classes, we need it)

(defn read-transitions
  "Build the transitions from a sequence of elements, based
   on a sliding window of size `window-size`"
  [token-seq window-size]
  (persistent!
    (reduce
      (fn [transitions [k v]]
        (assoc! transitions k
          (update (get transitions k {}) v (fnil + 0) 1)))
      (transient {})
      (partition 2 1 (partition window-size 1 token-seq)))))

(deftest test-read-transitions
  (let [token-seq (split-words "a b c a b d")]
    (is (= '{("a") {("b") 2}
             ("b") {("c") 1 ("d") 1}
             ("c") {("a") 1}}
          (read-transitions token-seq 1)))
    (is (= '{("a" "b") {("b" "c") 1 ("b" "d") 1}
             ("b" "c") {("c" "a") 1}
             ("c" "a") {("a" "b") 1}}
          (read-transitions token-seq 2)))
    ))


;; Transformation into a markov chain

(defn weighted-start-elements
  [transitions]
  (letfn [(weight-key [[curr nexts]] [curr (count nexts)])]
    (into {} (map weight-key) transitions)))

(defn transition->markov-chain
  "Transform a sequence of weighted transitions into a markovian process
   Each weighted map is transformed into a generator"
  [transitions]
  {:initial-gen (weighted-keys->gen (weighted-start-elements transitions))
   :words->gen (map-values weighted-keys->gen transitions)
   })


;; Random generation based on initial values

(defn random-jump
  [markov-chain curr]
  (->
    (get (:words->gen markov-chain) curr)
    (or (:initial-gen markov-chain))
    run-gen))

(defn random-walk
  "Returns an infinite sequence of element, based on the markovian chain
   The optional second argument allows to provide a starting element"
  ([markov-chain]
    (let [start (run-gen (:initial-gen markov-chain))]
      (random-walk markov-chain start)))
  ([markov-chain start]
    (letfn [(go [curr]
              (cons (first curr)
                (lazy-seq
                  (go (random-jump markov-chain curr)))))]
      (go start))))


;; Test case

(defn run-test
  []
  (let [text (slurp "./src/cljc/algorithms_clj/markov-chain-input.edn")
        markov (transition->markov-chain (read-transitions (split-words text) 1))]
    (str/join " "
      (take 100 (random-map-walk markov)))
    ))
