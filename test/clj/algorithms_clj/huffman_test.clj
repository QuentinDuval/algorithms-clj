(ns algorithms-clj.huffman-test
  (:require
    [algorithms-clj.huffman :refer :all]
    [clojure.test :refer :all]
    [clojure.test.check :as tc]
    [clojure.test.check.clojure-test :refer [defspec]]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [midje.sweet :refer :all]
    ))


;; -------------------------------------------------------

(defn to-encoding-map
  [value-frequency-pairs]
  (huffman-tree->encoding-map (make-huffman-tree value-frequency-pairs)))

(deftest test-huffman-encoding-map
  (facts "About the generation of huffman encoding map"
    (fact "No inputs leads to a nil tree and no encoding"
      (make-huffman-tree []) => nil
      (to-encoding-map []) => {}
      )
    (fact "Signal with no information still generates a encoding map"
      (make-huffman-tree [["a" 1]]) => is-node?
      (to-encoding-map [["a" 1]]) => {"a" [0]}
      )
    (fact "Two symbols always leads to a map with single digit codes"
      (to-encoding-map [["a" 1] ["b" 2]]) => {"a" [0] "b" [1]}
      )
    (fact "With 3 inputs, the most common symbol has the least number of digit"
      (to-encoding-map [["a" 1] ["b" 4] ["c" 2]]) => {"a" [0 0], "b" [1], "c" [0 1]}
      )
    (fact "In general, the least frequent symbol get the more digit"
      (to-encoding-map (hash-map :a 1 :b 2 :c 3 :d 3 :e 5))
      => {:a [0 0 0], :b [0 0 1], :c [0 1], :d [1 0], :e [1 1]}
      )
    ))

;; -------------------------------------------------------

(defn starts-with
  [lhs rhs]
  (every? true? (map = lhs rhs)))

(defn no-prefix
  [codes]
  (let [pair-sorted-codes (partition 2 1 (sort codes))]
    (nil? (some #(apply starts-with %) pair-sorted-codes))
    ))

(def no-code-is-prefix-of-other
  (prop/for-all [original (gen/vector gen/int 1 100)]
    (let [code (to-encoding-map (frequencies original))]
      (no-prefix (map second code))
      )))

(defspec test-no-code-is-prefix-of-other
  100 ;; Will call (tc/quick-check 100 decode-code-cycle)
  no-code-is-prefix-of-other)

;; -------------------------------------------------------

(deftest encoding-with-provided-tree
  (fact "Using an external huffman tree takes precedence of signal frequency" 
    (let [freqs (hash-map :a 1 :b 2 :c 3 :d 3 :e 5)
          htree (make-huffman-tree freqs)]
      (encode-with htree [:a :b :c :d :e]) => [0 0 0 0 0 1 0 1 1 0 1 1]
      (decode htree [0 0 0 0 0 1 0 1 1 0 1 1]) => [:a :b :c :d :e]
      ))
  (fact "Providing no huffman tree will use the signal frequency"
    (let [freqs (frequencies [:a :b :c :d :e])
          htree (make-huffman-tree freqs)]
      (first (encode [:a :b :c :d :e])) => htree
      (second (encode [:a :b :c :d :e])) => (encode-with htree [:a :b :c :d :e])
      ))
  (fact "Encoding an empty signal should give back an empty signal"
    (second (encode [:a :a :a :a])) => [0 0 0 0]
    ))

;; -------------------------------------------------------

(def decoding-encoded-message-should-return-the-orginal
  (prop/for-all [original (gen/vector gen/int 1 100)]
    (let [[code encoded] (encode original)
          decoded (decode code encoded)]
      (= original decoded))))

(defspec test-decoding-encoded-message-should-return-the-orginal
  100 ;; Will call (tc/quick-check 100 decode-code-cycle)
  decode-code-cycle)

(run-tests)
