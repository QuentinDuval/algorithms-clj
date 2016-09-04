(ns algorithms-clj.huffman
  (:require
    [algorithms-clj.priority-map-utils :as prio-utils]
    [algorithms-clj.utils :as utils] 
    [clojure.data.priority-map :as prio]
    [clojure.set :as set]
    ))


;; -----------------------------------------------------------
;; Build the Huffman tree
;; -----------------------------------------------------------

(defn make-leaf [value] [::leaf value])
(defn make-node [lh rh] [::node lh rh])
(defn is-leaf? [node] (= (first node) ::leaf))
(defn is-node? [node] (= (first node) ::node))

(defn leaf-val [node] {:pre (is-leaf? node)} (second node))
(defn lhs-node [node] {:pre (is-node? node)} (nth node 1))
(defn rhs-node [node] {:pre (is-node? node)} (nth node 2))

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

(defn make-huffman-tree
  "Build the huffman tree from a list of (value, frequence) pairs"
  [value-frequency-pairs]
  (->>
    value-frequency-pairs
    (into (prio/priority-map) (utils/map-first make-leaf))
    merge-nodes-by-lowest-frequency))


;; -----------------------------------------------------------
;; Encoding
;; -----------------------------------------------------------

(defn- huffman-tree->encoding-map
  "Traverse the tree to get the symbol to code map"
  [tree]
  (defn go [path node] ;; TODO - optimize this by removing the recursion
    (cond
      (is-leaf? node) [(leaf-val node) path]
      (is-node? node) (into
                        (go (conj path 0) (lhs-node node))
                        (go (conj path 1) (rhs-node node)))
      ))
  (apply hash-map (go [] tree))) ;; TODO - Test check: verify that there are no code prefix of another

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
          (let [{:keys [values] :as next-branch} ((if (zero? input) :lhs :rhs) @branch)]
            (vreset! branch next-branch)
            (if (= 1 (count values))
              (do (vreset! branch huffman-tree)
                  (xf result (first values)))
              result)))
        ))))

(defn decode
  "Decode a stream of inputs, provided the huffman tree as first parameter"
  [huffman-tree inputs]
  (into [] (huffman-xf huffman-tree) inputs))


;; -----------------------------------------------------------
;; Test data
;; -----------------------------------------------------------

(def m (sorted-map :a 1 :b 2 :c 3 :d 3 :e 5))
(def t (make-huffman-tree m))

(defn tests []
  ;; TODO - Symetric testings with test.checks
  (prn (encode-with t [:a :b :c :d :e]))
  (prn (second (encode [:a :b :c :d :e])))
  (prn (decode t [0 0 0 0 0 1 0 1 1 0 1 1]))
  )



