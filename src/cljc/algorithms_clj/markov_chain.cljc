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


;; Construct the transitions
;; TODO - have a specific transducer for this

(defn read-transitions
  [token-seq memory]
  (persistent!
    (reduce
      (fn [transitions [k v]]
        (assoc! transitions k
          (update (get transitions k {}) v (fnil + 0) 1)))
      (transient {})
      (partition 2 1 (partition memory 1 token-seq)))))


;; Random generation based on initial values

(defn random-jump
  [transitions weighted-keys curr]
  (let [possibles (or (get transitions curr) weighted-keys)]
    (weighted-rand possibles)))

(defn random-walk-from
  [transitions curr]
  (let [weighted-keys (into {}
                        (map (fn [[curr nexts]]
                               [curr (count nexts)]))
                        transitions)]
    (letfn [(go [curr]
              (cons (first curr)
                (lazy-seq
                  (go (random-jump transitions weighted-keys curr)))))]
      (go curr))))

(defn random-map-walk
  [transitions]
  ;; TODO - weighted keys there too... need better data structure than just a map
  (random-walk-from transitions
    (rand-nth (keys transitions))))


;; Test case

(defn run-test
  []
  (let [text (slurp "./src/cljc/algorithms_clj/markov-chain-input.edn")
        markov (read-transitions (split-words text) 1)]
    (str/join " "
      (take 100 (random-map-walk markov)))
    ))
