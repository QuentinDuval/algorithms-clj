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

(defn weight [[val weight]] weight)

(defn enumerated-dist->aliases
  [weighted-pairs]
  (let [sum-weights (transduce (map weight) + weighted-pairs)
        avg-weight (/ sum-weights (count weighted-pairs))
        balanced (filter #(= (weight %) avg-weight) weighted-pairs)]

    (println avg-weight)
    (loop [lowers (filter #(< (weight %) avg-weight) weighted-pairs)
           higher (filter #(> (weight %) avg-weight) weighted-pairs)
           result (map (fn [[val weight]] [val val 1]) balanced)]

      (println lowers)
      (println higher)
      (println result)

      (cond
        ; End of the algorithm
        (empty? lowers) result

        ; Compensate lower by higher
        (not (empty? higher))
        (let [[[lo wo] & lowers] lowers
              [[hi wi] & higher] higher
              result (conj result [lo hi (/ wo avg-weight)])
              new-wi (- wi (- avg-weight wo))
              new-hi [hi new-wi]]
          (cond
            (< new-wi avg-weight) (recur (conj lowers new-hi) higher result)
            (< avg-weight new-wi) (recur lowers (conj higher new-hi) result)
            :else (recur lowers higher (conj result [hi hi 1]))))

        ; Last two elements should sum to average wieght
        (= 2 (count lowers))
        (let [[[lo wo] [hi wi]] lowers]
          (conj result [lo hi (/ wo avg-weight)]))

        ; should not happen
        (= 1 (count lowers)) nil))))

(defn alias-method-gen
  [weighted-pairs]
  (let [aliases (enumerated-dist->aliases weighted-pairs)]
    (fn alias-gen []
      (let [[v1 v2 p] (rand-nth aliases)]
        (if (< (rand) p) v1 v2)))
    ))

(deftest test-enumarated-dist->aliases
  (are [expected input] (= expected (enumerated-dist->aliases input))
    '([:c :c 1] [:b :c 3/4] [:a :c 3/4]) {:a 1 :b 1 :c 2}
    '([:a :a 1] [:b :b 1] [:c :c 1]) {:a 1 :b 1 :c 1}
    '([:B :A 0.80
       :A :C 0.92
       :C :F 0.12
       :E :F 0.48]) {:A 0.28 :B 0.20 :C 0.05 :E 0.12 :F 0.35}
    ))


;; To Weighted Generators

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
  ;; TODO - use train or better vocabulary for this
  "Build the transitions from a sequence of elements, based
   on a sliding window of size `window-size`"
  [token-seq window-size]
  (reduce
    (fn [transitions [k v]]
      (update-in transitions [k v] (fnil + 0) 1))
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
      (take 100 (random-map-walk markov)))
    ))
