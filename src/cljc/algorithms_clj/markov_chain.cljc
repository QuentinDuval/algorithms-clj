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


;; Construct the transitions
;; TODO - have a specific transducer for this
;; TODO - The weighting is based on duplicates...

(defn read-transitions
  [token-seq memory]
  (persistent!
    (reduce
      (fn [transitions [k v]]
        (assoc! transitions k
          (conj (get transitions k []) v)))
      (transient {})
      (partition 2 1 (partition memory 1 token-seq)))))


;; Random generation based on initial values

(defn random-jump
  [transitions curr]
  (let [possibles (or (get transitions curr) (keys transitions))]
    (rand-nth possibles)))

(defn random-walk-from
  [transitions curr]
  (letfn [(go [curr]
            (cons (first curr)
              (lazy-seq
                (go (random-jump transitions curr)))))]
    (go curr)))

(defn random-map-walk
  [transitions]
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
