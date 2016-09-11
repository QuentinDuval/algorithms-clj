(ns algorithms-clj.huffman
  (:require
    [algorithms-clj.priority-map-utils :as prio-utils]
    [algorithms-clj.utils :as utils] 
    [clojure.data.priority-map :as prio]
    [clojure.set :as set]
    ))



;; -----------------------------------------------------------
;; Helpers to build a binary tree 
;; -----------------------------------------------------------

(defn make-leaf [value] [::leaf value])
(defn make-node [lh rh] [::node lh rh])
(defn is-leaf? [node] (= (first node) ::leaf))
(defn is-node? [node] (= (first node) ::node))

(defn leaf-val [node] {:pre (is-leaf? node)} (second node))
(defn children [node] {:pre (is-node? node)} (rest node))
(defn child-at [node n] {:pre (is-node? node)} (nth node (inc n)))


;; -----------------------------------------------------------
;; Build the Huffman tree
;; -----------------------------------------------------------

(defn- build-leaf-priority-queue
  [value-frequency-pairs]
  (into (prio/priority-map) (utils/map-first make-leaf) value-frequency-pairs))

(defn- merge-nodes-by-lowest-frequency
  "Build the huffman tree from the priority map"
  [heap]
  (first
    (utils/reduce-to-single-value [heap]
      (let [[poped tail] (prio-utils/pop-n heap 2)
            [a prio-a] (poped 0)
            [b prio-b] (poped 1)]
        (assoc tail (make-node a b) (+ prio-a prio-b)))
      )))

(defn- adapt-root-leaf-tree
  [huffman-tree]
  (if (is-leaf? huffman-tree)
    (make-node huffman-tree nil)
    huffman-tree))

(defn make-huffman-tree
  "Build the huffman tree from a list of (value, frequence) pairs"
  [value-frequency-pairs]
  (->>
    (build-leaf-priority-queue value-frequency-pairs)
    (merge-nodes-by-lowest-frequency)
    (adapt-root-leaf-tree)
    ))


;; -----------------------------------------------------------
;; Encoding
;; -----------------------------------------------------------

(defn huffman-tree->encoding-map
  "Traverse the tree to get the symbol to code map"
  [tree]
  (defn go [path node continue] ;; Use continuation to avoid recursion
    (cond
      (is-leaf? node) (continue [(leaf-val node) path])
      (is-node? node) 
      (go (conj path 0) (child-at node 0)
        (fn [lhs]
          (if (child-at node 1)
            (go (conj path 1) (child-at node 1)
              (fn [rhs]
                (continue (concat lhs rhs))
                ))
            (continue lhs))
          ))
      ))
  (apply hash-map (go [] tree identity)))

(defn encode-with
  "Encode a stream of values with the decoder provided as first parameter"
  [huffman-tree values]
  (let [encoder (huffman-tree->encoding-map huffman-tree)]
    (into [] (mapcat encoder) values)))

(defn encode
  "Encode a stream, using the stream frequences to build the huffman tree"
   ;; TODO - Encode the tree as well
  [values]
  (let [t (-> values frequencies make-huffman-tree)]
    [t (encode-with t values)]
    ))


;; -----------------------------------------------------------
;; Decoding
;; -----------------------------------------------------------

(defn- huffman-xf
  "Transducer step function to decode a huffman stream of values"
  [huffman-tree]
  (fn [xf]
    (let [branch (volatile! huffman-tree)]
      (fn step-fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
          (let [next-branch (child-at @branch input)]
            (if (is-leaf? next-branch)
              (do
                (vreset! branch huffman-tree)
                (xf result (leaf-val next-branch)))
              (do
                (vreset! branch next-branch)
                result))
            ))
        ))))

(defn decode
  "Decode a stream of inputs, provided the huffman tree as first parameter"
  [huffman-tree inputs]
  (into [] (huffman-xf huffman-tree) inputs))

