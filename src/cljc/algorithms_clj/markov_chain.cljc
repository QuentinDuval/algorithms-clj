(ns cljc.algorithms-clj.markov-chain
  (:require
    [clojure.string :as str]))


;; Different ways to extract the information from text

(defn split-words
  [text]
  (str/split text #" "))

(defn split-class-name
  [class-name]
  (map (fn [[a b]]
         (str (apply str a) (apply str b)))
    (partition 2
      (partition-by #(Character/isUpperCase %) class-name))))


;; Weighted choice
;; TODO - use a better algorithm... this one is linear
;; TODO - make a preparation phase for this algo as well... this is not good

(defn weighted-rand
  "Given a map of generators and weights, return a value from one of
   the generators, selecting generator based on weights."
  [m]
  (let [weights (reductions + (vals m))
        total (last weights)
        choices (map vector (keys m) weights)]
    (let [choice (rand-int total)]
      (loop [[[val w] & more] choices]
        (if (< choice w) val (recur more))))))


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
    {:weighted-keys (into {} (map weight-key) transitions)
     :transitions transitions}))


;; Random generation based on initial values

(defn random-jump
  [{:keys [weighted-keys transitions]} curr]
  (let [possibles (or (get transitions curr) weighted-keys)]
    (weighted-rand possibles)))

(defn random-walk-from
  [markov-chain curr]
  (letfn [(go [curr]
            (cons (first curr)
              (lazy-seq
                (go (random-jump markov-chain curr)))))]
    (go curr)))

(defn random-map-walk
  [markov-chain]
  (random-walk-from markov-chain
    (weighted-rand (:weighted-keys markov-chain))))


;; Test case

(defn run-test
  []
  (let [text (slurp "./src/cljc/algorithms_clj/markov-chain-input.edn")
        markov (transition->markov-chain (read-transitions (split-words text) 1))]
    (str/join " "
      (take 100 (random-map-walk markov)))
    ))
