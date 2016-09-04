(ns algorithms-clj.priority-map-utils
  (:require
    [clojure.data.priority-map :as prio]
    [clojure.set :as set] 
    ))


(defn pop-n
  "Pop N elements from the priority queue"
  [heap n]
  (loop [vals []
         tail heap
         i n]
    (if (= 0 i)
      [vals tail]
      (recur
        (conj vals (peek tail))
        (pop tail)
        (dec i)))
    ))

