(ns algorithms-clj.huffman-test
  (:require
    [algorithms-clj.huffman :refer :all]
    [clojure.test :refer :all]
    [clojure.test.check :as tc]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    ))


;; -------------------------------------------------------

(defn- to-encoding-map
  [value-frequency-pairs]
  (huffman-tree->encoding-map (make-huffman-tree value-frequency-pairs)))

(deftest make-huffman-tree-test
  "Tests on the encoding of the huffman tree"
  
  (testing "Empty input leads to empty huffman tree"
    (is (= nil (make-huffman-tree [])))
    (is (= {} (to-encoding-map []))))
  
  (testing "Single generate a one symbol alphabet"
    (let [inputs [["a" 1]]]
      (is (= {"a" [0]} (to-encoding-map inputs)))
      ))
  
  (testing "Two inputs generate two one digit symbols"
    (let [inputs [["a" 1] ["b" 2]]]
      (is (= {"a" [0] "b" [1]} (to-encoding-map inputs)))
      ))
   
  (testing "three inputs - coding map"
    (let [inputs [["a" 1] ["b" 4] ["c" 2]]
          expected {"a" [0 0], "b" [1], "c" [0 1]}]
      (is (= expected (to-encoding-map inputs)))
      ))
  )

;; -------------------------------------------------------

(def decode-code-cycle
  (prop/for-all [original (gen/vector gen/int 1 100)]
    (let [[code encoded] (encode original)
          decoded (decode code encoded)]
      (= original decoded))
    ))

(tc/quick-check 100 decode-code-cycle)
(run-tests)
