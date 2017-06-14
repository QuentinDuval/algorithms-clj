(ns algorithms-clj.perf-recons
  (:require [criterium.core :as perf]))


(set! *warn-on-reflection* true)

(defn sum-adjacents-1
  [[x y & xs]]
  (cons (+ x y)
    (if-not (empty? xs)
      (sum-adjacents (cons y xs))
      (list))))

(defn sum-adjacents-2
  [[x & xs]]
  (let [[y & ys] xs]
    (cons (+ x y)
      (if-not (empty? ys) (sum-adjacents xs) (list)))))

; -----------------------------------------------

(defn run-bench*
  [name f n]
  (println "\n" name ": ------------------------------")
  (perf/quick-bench (f n)))

(defmacro run-bench
  [[f n]]
  `(run-bench* ~(name f) ~f ~n))

(defn run-benches
  []
  (let [xs (vec (range 1000))]
    (run-bench (sum-adjacents-1 xs))
    (run-bench (sum-adjacents-2 xs))
    ))
