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
  (->
    (priority-map)
    (into (map make-leaf) inputs)
    (make-tree-impl) (first)))


;; -----------------------------------------------------------
;; Encoding
;; -----------------------------------------------------------

(defn ^:private get-bits
  [huffman-tree val]
  (defn get-bits-impl [{:keys [values lhs rhs]} dirs]
    (cond
      (= 1 (count values)) dirs
      (-> lhs :values val) (recur lhs (conj dirs 0))
      (-> rhs :values val) (recur rhs (conj dirs 1))
      ))
  (get-bits-impl huffman-tree []))

(defn tree->decoder
  "Build a decoder with memoization"
  [huffman-tree]
  (memoize #(get-bits huffman-tree %)))

(defn encode-with
  "Encode a stream of values with the decoder provided as first parameter"
  [decoder values]
  (transduce (mapcat decoder) conj [] values))

(defn encode
  "Encode a stream, using the stream frequences to build the huffman tree"
  [values]
  (-> (frequencies values)
      (make-tree)
      (tree->decoder)
      (encode-with values)))


;; -----------------------------------------------------------
;; Decoding
;; -----------------------------------------------------------

(defn ^:private decode-one
  [{:keys [values lhs rhs]} bits]
  (cond
    (= 1 (count values)) [(first values) bits]
    (= 0 (first bits)) (recur lhs (rest bits))
    (= 1 (first bits)) (recur rhs (rest bits))
    ))

(defn ^:private decode-bits
  [huffman-tree inputs]
  (loop [bits inputs
         result []]
    (if (empty? bits)
      result
      (let [[c tail-bits] (decode-one huffman-tree bits)]
        (recur tail-bits (conj result c))
        ))
    ))


;; -----------------------------------------------------------
;; Test data
;; -----------------------------------------------------------

(def m (sorted-map :a 1 :b 2 :c 3 :d 3 :e 5))
(def t (make-tree m))

(defn tests []
  (prn (encode-with (tree->decoder t) [:a :b]))
  (prn (encode [:a :b]))
  (prn (decode-bits t [0 1 0 0 1 1]))
  )



