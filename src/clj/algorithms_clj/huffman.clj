(ns algorithms-clj.huffman
  (:require
    [algorithms-clj.priority-map-utils :as prio-utils]
    [clojure.data.priority-map :as prio]
    [clojure.set :as set] 
    ))


;; -----------------------------------------------------------
;; Build the Huffman tree
;; -----------------------------------------------------------

(defn- make-leaf
  "Create a leaf for the huffman code tree"
  [[value freq]]
  [{:values #{value}} freq])

(def ^:private node-value first)
(def ^:private node-frequency second)

(defn- merge-nodes
  "Build an intermediary node from two sub-trees"
  [[c1 w1] [c2 w2]]
  [{:values (set/union (:values c1) (:values c2))
    :lhs c1
    :rhs c2}
   (+ w1 w2)])

(defn- merge-node-by-lowest-frequency
  "Build the huffman tree from the priority map"
  [heap]
  (if (= 1 (count heap))
    (first heap)
    (let [[[a b] rest] (prio-utils/pop-n heap 2)]
      (recur (conj rest (merge-nodes a b)))
      )))

(defn make-tree
  "Build the huffman tree from a list of (value, frequence) pairs"
  [value-frequency-pairs]
  (->>
    (into (prio/priority-map) (map make-leaf) value-frequency-pairs)
    merge-node-by-lowest-frequency
    node-value))


;; -----------------------------------------------------------
;; Encoding
;; -----------------------------------------------------------

(defn- get-bits
  [huffman-tree val]
  (defn get-bits-impl [{:keys [values lhs rhs]} directions]
    (cond
      (= 1 (count values)) directions
      (-> lhs :values val) (recur lhs (conj directions 0))
      (-> rhs :values val) (recur rhs (conj directions 1))
      ))
  (get-bits-impl huffman-tree []))

(defn encode-with
  "Encode a stream of values with the decoder provided as first parameter"
  [huffman-tree values]
  (let [encoder (memoize #(get-bits huffman-tree %))]
    (into [] (mapcat encoder) values)))

(defn encode
  "Encode a stream, using the stream frequences to build the huffman tree"
   ;; TODO - Encode the tree as well
  [values]
  (let [t (-> values frequencies make-tree)]
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
(def t (make-tree m))

(defn tests []
  ;; TODO - Symetric testings with test.checks
  (prn (encode-with t [:a :b]))
  (prn (second (encode [:a :b])))
  (prn (decode t [0 1 0 0 1 1]))
  )



