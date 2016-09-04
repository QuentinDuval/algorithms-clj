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

(defn- make-leaf
  "Create a leaf for the huffman code tree"
  [[value freq]]
  [{:values #{value}} freq])

(def ^:private node-value first)
(def ^:private node-frequency second)
(defn- is-leaf [node] (= 1 (-> node :values count)))

(defn- merge-nodes
  "Build an intermediary node from two sub-trees"
  [lhs rhs]
  {:values (set/union (:values lhs) (:values rhs))
   ;; TODO - We do not need these values in intermediate nodes => just traverse tree for encoding
   :lhs lhs
   :rhs rhs})

(defn- merge-nodes-by-lowest-frequency
  "Build the huffman tree from the priority map"
  [heap]
  (utils/reduce-to-single-value [heap]
    (let [[poped tail] (prio-utils/pop-n heap 2)
          [a prio-a] (poped 0)
          [b prio-b] (poped 1)]
      (assoc tail (merge-nodes a b) (+ prio-a prio-b)))
    ))

(defn make-huffman-tree
  "Build the huffman tree from a list of (value, frequence) pairs"
  [value-frequency-pairs]
  (->>
    value-frequency-pairs
    (into (prio/priority-map) (map make-leaf)) ;; TODO - Instead of a priority map, you can use two queues
    merge-nodes-by-lowest-frequency
    node-value))


;; -----------------------------------------------------------
;; Encoding
;; -----------------------------------------------------------

(defn- get-bits
  "Encode the 'value' with the huffman-tree provided as parameter" 
  [huffman-tree value]
  (defn get-bits-impl [{:keys [lhs rhs] :as node} directions]
    (cond
      (is-leaf node) directions
      (-> lhs :values value) (recur lhs (conj directions 0))
      (-> rhs :values value) (recur rhs (conj directions 1))
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
  (prn (encode-with t [:a :b]))
  (prn (second (encode [:a :b])))
  (prn (decode t [0 1 0 0 1 1]))
  )



