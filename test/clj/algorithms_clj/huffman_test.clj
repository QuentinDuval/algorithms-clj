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

(deftest huffman-code-generation
  (facts "About the generation of huffman encoding map"
    (fact "No inputs leads to a nil tree and no encoding"
      (make-huffman-tree []) => nil
      (to-encoding-map []) => {}
      )
    (fact "Signal with no information still generates a encoding map"
      (to-encoding-map [["a" 1]]) => {"a" [0]}
      )
    (fact "Two symbols always leads to a map with single digit codes"
      (to-encoding-map [["a" 1] ["b" 2]]) => {"a" [0] "b" [1]}
      )
    (fact "With 3 inputs, the most common symbol has the least number of digit"
      (to-encoding-map [["a" 1] ["b" 4] ["c" 2]]) => {"a" [0 0], "b" [1], "c" [0 1]}
      )
    ))

;; -------------------------------------------------------

(def decode-code-cycle
  (prop/for-all [original (gen/vector gen/int 1 100)]
    (let [[code encoded] (encode original)
          decoded (decode code encoded)]
      (= original decoded))))

(defspec decoding-encoded-message-should-return-the-orginal
  100 ;; Will call (tc/quick-check 100 decode-code-cycle)
  decode-code-cycle)

(run-tests)
