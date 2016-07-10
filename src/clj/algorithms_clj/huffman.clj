(ns algorithms-clj.huffman
  (:require
    [clojure.data.priority-map :as prio :refer [priority-map]]
    ))


;; -----------------------------------------------------------
;; Build the Huffman tree
;; -----------------------------------------------------------

(defn ^:private make-leaf
  "Create a leaf for the huffman code tree"
  [[value freq]]
  [{:values #{value}} freq])

(defn ^:private make-node
  "Build an intermediary node from two sub-trees"
  [[c1 w1] [c2 w2]]
  (let [vals (into (:values c1) (:values c2))]
    [{:values vals :lhs c1 :rhs c2} (+ w1 w2)]
    ))

(defn ^:private merge-lowest
  "Merge the two lowest priority elements"
  [heap]
  (let [rest (pop heap)]
    (conj (pop rest)
      (make-node (peek heap) (peek rest))
      )))

(defn ^:private make-tree-impl
  "Build the huffman tree from the priority map"
  [heap]
  (if (< 1 (count heap))
    (recur (merge-lowest heap))
    (first heap)))

(defn make-tree
  "Build the huffman tree from a list of (value, frequence) pairs"
  [inputs]
  (-> (into (priority-map) (map make-leaf) inputs)
      (make-tree-impl) (first)))


;; -----------------------------------------------------------
;; Encoding
;; -----------------------------------------------------------




;; -----------------------------------------------------------
;; Decoding
;; -----------------------------------------------------------



;; -----------------------------------------------------------
;; Test data
;; -----------------------------------------------------------

(def m (sorted-map :a 1 :b 2 :c 3 :d 3 :e 5))



