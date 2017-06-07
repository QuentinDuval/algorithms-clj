(ns cljc.algorithms-clj.markov-chain
  (:require
    [clojure.string :as str]
    [clojure.test :as test :refer [deftest is are testing]]
    [algorithms-clj.alias-method :refer [enumerated-distribution-gen]]
    [algorithms-clj.utils :refer [map-values]]
    ))


; -----------------------------------------------------------------------------
; Different ways to extract the information from text
; -----------------------------------------------------------------------------

(defn split-words
  [text]
  (str/split text #" "))

#_(defn split-class-name
    [class-name]
    (map (fn [[a b]]
           (str (apply str a) (apply str b)))
      (partition 2
        (partition-by #(Character/isUpperCase %) class-name))))


; -----------------------------------------------------------------------------
; To Weighted Generators
; -----------------------------------------------------------------------------

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


; -----------------------------------------------------------------------------
; Transformation into a markov chain
; -----------------------------------------------------------------------------

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


; -----------------------------------------------------------------------------
; Walking the Markov Chain
; -----------------------------------------------------------------------------

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


; -----------------------------------------------------------------------------
; Unit tests
; -----------------------------------------------------------------------------

(deftest test-read-transitions
  (let [token-seq (split-words "a b c a b d")]
    (is (= {["a"] {["b"] 2},
            ["b"] {["c"] 1, ["d"] 1},
            ["c"] {["a"] 1}}
          (read-transitions token-seq 1)))
    (is (= {["a" "b"] {["b" "c"] 1, ["b" "d"] 1},
            ["b" "c"] {["c" "a"] 1},
            ["c" "a"] {["a" "b"] 1}}
          (read-transitions token-seq 2)))
    ))


; -----------------------------------------------------------------------------
; Integration tests
; -----------------------------------------------------------------------------

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
