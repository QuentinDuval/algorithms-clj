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


;; Construct the markov-chain
;; TODO - have a specific transducer for this
;; TODO - Allow to feed the transitions several times (for classes, we need it)

(defn read-transitions
  [token-seq memory]
  (persistent!
    (reduce
      (fn [transitions [k v]]
        (assoc! transitions k
          (update (get transitions k {}) v (fnil + 0) 1)))
      (transient {})
      (partition 2 1 (partition memory 1 token-seq)))))


;; Transformation into a markov chain

(defn transition->markov-chain
  [transitions]
  (letfn [(weight-key [[curr nexts]] [curr (count nexts)])]
    ; TODO - weighted-keys is incorrect: should sum the weights of transitions as well
    {:initial-gen (weighted-keys->gen
                    (into {} (map weight-key) transitions))
     :words->gen (map-values weighted-keys->gen transitions)
     }))


;; Random generation based on initial values

(defn run-gen [gen] (gen))

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
