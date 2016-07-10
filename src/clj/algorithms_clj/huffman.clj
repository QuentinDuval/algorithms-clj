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

(defn encode-with
  "Encode a stream of values with the decoder provided as first parameter"
  [huffman-tree values]
  (let [encoder (memoize #(get-bits huffman-tree %))]
    (into [] (mapcat encoder) values)))

(defn encode
  "Encode a stream, using the stream frequences to build the huffman tree"
  [values]
  (let [t (-> values frequencies make-tree)]
    [t (encode-with t values)])) ;; TODO - Encode the tree as well


;; -----------------------------------------------------------
;; Decoding
;; -----------------------------------------------------------

(defn ^:private huffman-xf
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

(defn decode ;; TODO - The transducer could be used to do pipe-line parallelism
  "Decode a stream of inputs, provided the huffman tree as first parameter"
  [huffman-tree inputs]
  (into [] (huffman-xf huffman-tree) inputs))


;; -----------------------------------------------------------
;; Test data
;; -----------------------------------------------------------

(def m (sorted-map :a 1 :b 2 :c 3 :d 3 :e 5))
(def t (make-tree m))

(defn tests []
  ;; TODO - Symetric testings with test.checks
  (prn (encode-with t [:a :b]))
  (prn (second (encode [:a :b])))
  (prn (decode t [0 1 0 0 1 1]))
  )



