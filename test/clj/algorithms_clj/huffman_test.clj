(ns algorithms-clj.huffman-test
  (:require
    [algorithms-clj.huffman :refer :all]
    [clojure.test :refer :all]
    [clojure.test.check :as tc]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    ))


;; -------------------------------------------------------

(deftest make-huffman-tree-test
  "Tests on the encoding of the huffman tree"
  
  (testing "empty input"
    (let [r (make-huffman-tree [])]
      (is (= nil r))
      ))
  
  (testing "one input"
    (let [r (make-huffman-tree [["a" 1]])]
      (is (is-leaf? r))
      ))
  
  (testing "two inputs"
    (let [r (make-huffman-tree [["a" 1] ["b" 2]])]
      (is (= "a" (-> r lhs-node leaf-val)))
      (is (= "b" (-> r rhs-node leaf-val)))
      ))
  
  (testing "three inputs"
    (let [r (make-huffman-tree [["a" 1] ["b" 4] ["c" 2]])
          w (flatten r)]
      (is (= [:algorithms-clj.huffman/node
              :algorithms-clj.huffman/node
              :algorithms-clj.huffman/leaf "a"
              :algorithms-clj.huffman/leaf "c"
              :algorithms-clj.huffman/leaf "b"] w))
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
