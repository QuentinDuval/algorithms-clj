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
  
  (testing "one input should not generate a leaf" ;; TODO => make this a property
    (let [r (make-huffman-tree [["a" 1]])]
      (is (is-node? r))
      (is (is-leaf? (lhs-node r)))
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

;;(huffman-tree->encoding-map
;;  (make-huffman-tree [[:a 1] [:b 2] [:c 3]]))
;;{:c [1], :b [0 1], :a [0 0]}


;; -------------------------------------------------------

(def decode-code-cycle
  (prop/for-all [original (gen/vector gen/int 1 100)]
    (let [[code encoded] (encode original)
          decoded (decode code encoded)]
      (= original decoded))
    ))

(tc/quick-check 100 decode-code-cycle)
(run-tests)
