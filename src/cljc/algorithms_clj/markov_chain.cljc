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
; https://stackoverflow.com/questions/6409652/random-weighted-selection-in-java
; https://oroboro.com/non-uniform-random-numbers/

(defn build-alias-array
  "Preprocessing phase of the Alias Algorithm
   (Build the array of the alias method in O(N log N) complexity
   - Input: a sequence of pairs [value associated-weighted]
   - Output: an array of tuples [value-1 value-2 p1-over-p2]"
  [weighted-pairs]
  (if (< (count weighted-pairs) 2)
    (let [[val _] (first weighted-pairs)]
      [[val val 1]])
    (let [sum-weights (transduce (map second) + weighted-pairs)
          avg-weight (/ sum-weights (dec (count weighted-pairs)))]
      (loop [weighted-pairs (into (sorted-set) (map (comp vec reverse)) weighted-pairs)
             result []]
        (if (<= 2 (count weighted-pairs))
          (let [[w-least v-least :as least] (first weighted-pairs)
                [w-most v-most :as most] (last weighted-pairs)
                weighted-pairs (disj weighted-pairs least most)
                remaining-weight (- w-most (- avg-weight w-least))
                result (conj result [v-least v-most (/ w-least avg-weight)])]
            (if (zero? remaining-weight)
              (recur weighted-pairs result)
              (recur (conj weighted-pairs [remaining-weight v-most]) result)
              ))
          result)))))

(defn enumerated-distribution-gen
  [weighted-pairs]
  (let [aliases (build-alias-array weighted-pairs)]
    (fn alias-gen []
      (let [[v1 v2 p] (rand-nth aliases)]
        (if (< (rand) p) v1 v2)))
    ))

(deftest test-enumarated-dist->aliases
  (are [expected input] (= expected (build-alias-array input))
    [[nil nil 1]] {}
    [[:a :a 1]] {:a 1}
    [[:a :b 1/2]] {:a 1 :b 1}
    [[:a :b 1/3]] {:a 1 :b 2}
    [[:a :c 1/2] [:b :c 1/2]] {:a 1 :b 1 :c 2}
    [[:C :F 0.2]
     [:E :A 0.48]
     [:F :B 0.5999999999999999]
     [:B :A 0.3999999999999999]] {:A 0.28 :B 0.20 :C 0.05 :E 0.12 :F 0.35}
    [[:a :f 5/6]
     [:f :e 2/3]
     [:e :d 1/2]
     [:d :c 1/3]
     [:c :b 1/6]] {:a 1 :b 1 :c 1 :d 1 :e 1 :f 1}
    ))

; TODO - Property based testing: the output should be one less in size


;; To Weighted Generators

(defn slow-enumerated-distribution-gen
  [m]
  (let [weights (reductions + (vals m))
        total (last weights)
        choices (map vector (keys m) weights)]
    (fn []
      (let [choice (rand-int total)]
        (loop [[[val w] & more] choices]
          (if (< choice w) val (recur more)))))))

(defn weighted-keys->gen
  "Given a map of generators and weights, return a value from one of
   the generators, selecting generator based on weights."
  [m]
  (enumerated-distribution-gen m))

(defn run-gen [gen] (gen))


;; Construct the markov-chain
;; TODO - have a specific transducer for this
;; TODO - Allow to feed the transitions several times (for classes, we need it)

(defn read-transitions
  ;; TODO - use train or better vocabulary for this
  "Build the transitions from a sequence of elements, based
   on a sliding window of size `window-size`"
  [token-seq window-size]
  (reduce
    (fn [transitions [k v]]
      (update-in transitions [(vec k) (vec v)] (fnil + 0) 1))
    {}
    (partition 2 1 (partition window-size 1 token-seq))))

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
  "For each input element, compute its weight as the sum of the
   weight of each of its out-going elements"
  [transitions]
  (map-values #(transduce (map second) + %) transitions))

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
    (map first
      (iterate #(random-jump markov-chain %) start))))


;; Transitions from file
;; Read by streaming

(defn file->markov-transitions
  [file-path window-size]
  (with-open [r (clojure.java.io/reader file-path)]
    (read-transitions
      (eduction
        (mapcat split-words)
        (line-seq r))
      window-size)))


;; Test case

(def test-file-path
  "./src/cljc/algorithms_clj/markov-chain-input.edn")

(defn run-test
  []
  (let [markov (transition->markov-chain (file->markov-transitions test-file-path 1))]
    (str/join " "
      (take 100 (random-walk markov)))
    ))
