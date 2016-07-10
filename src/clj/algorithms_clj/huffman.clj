(ns algorithms-clj.huffman
  (:require
    [clojure.data.priority-map :as prio :refer [priority-map]]
    ))


(defn- make-leaf
  "Create a leaf for the huffman code tree"
  [[value freq]]
  [{:values #{value}} freq])

(defn- make-node
  "Build an intermediary node from two sub-trees"
  [[c1 w1 :as lhs]
   [c2 w2 :as rhs]]
  [{:values (into (:values c1) (:values c2))
    :childs [c1 c2]}
   (+ w1 w2)])

(defn- merge-lowest
  "Merge the two lowest priority elements"
  [heap]
  (let [x (peek heap)
        r (pop heap)
        y (peek r)]
    (conj (pop r) (make-node x y))))

(defn- make-tree-impl
  "Build the huffman tree from the priority map"
  [heap]
  (if (< 1 (count heap))
    (recur (merge-lowest heap))
    (first heap)))

(defn make-tree
  "Build the huffman tree from a list of (value, frequence) pairs"
  [inputs] 
  (let [heap (into (priority-map) (map make-leaf) inputs)]
    (first (make-tree-impl heap))
    ))
