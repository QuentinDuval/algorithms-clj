(ns algorithms-clj.perf-fibo
  (:require [criterium.core :as perf]))

; -----------------------------------------------

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

(defn fibo-iterate
  [^long n]
  (let [next-fib (fn [[a b]] [b (+ a b)])
        fibs (iterate next-fib [0N 1N])]
    (first (nth fibs n))))

(defn fibo-lazy-seq
  [^long n]
  (letfn [(fibs [a b] (cons a (lazy-seq (fibs b (+ a b)))))]
    (nth (fibs 0N 1N) n)))

(defn fibo-recur
  [^long n]
  (loop [curr 0N
         next 1N
         n n]
    (if-not (zero? n)
      (recur next (+ curr next) (dec n))
      curr)))

(defn fibo-trampoline
  [^long n]
  (letfn [(fibs [curr next ^long n]
            (if-not (zero? n)
              #(fibs next (+ curr next) (dec n))
              curr))]
    (trampoline (fibs 0N 1N n))))

(defn fibo-lazy-cat
  ; TODO - This is weird... it should be a var, but then...
  [n]
  (letfn [(fibs [] (lazy-cat [0N 1N] (map + (rest (fibs)) (fibs))))]
    (nth (fibs) n)))

; -----------------------------------------------

(defn fibo-local-vars
  [^long n]
  (with-local-vars [curr 0N
                    next 1N
                    iter n]
    (while (> @iter 0)
      (let [nnext (+ @curr @next)]
        (var-set curr @next)
        (var-set next nnext)
        (var-set iter (dec @iter))))
    @curr))

(defn fibo-volatile
  [^long n]
  (let [curr (volatile! 0N)
        next (volatile! 1N)
        iter (volatile! n)]
    (while (> @iter 0)
      (let [nnext (+ @curr @next)]
        (vreset! curr @next)
        (vreset! next nnext)
        (vswap! iter dec)))
    @curr))

;; This gets really weird now...

(defprotocol Advance
  (advance [this n]))

(deftype FiboType [^:unsynchronized-mutable curr
                   ^:unsynchronized-mutable next]
  Advance
  (advance [_ n]
    (loop [^long n n]
      (if-not (zero? n)
        (let [nnext (+ curr next)]
          (set! curr next)
          (set! next nnext)
          (recur (dec n)))))
    curr))

(defn fibo-with-type
  [^long n]
  (advance (FiboType. 0N 1N) n))

; -----------------------------------------------

(defn fibo-with-java
  [^long n]
  (javaalg.algorithms.PerfFiboJava/fibs n))

(defn fibo-recur-java-bigint
  [^long n]
  (loop [curr (BigInteger/valueOf 0)
         next (BigInteger/valueOf 1)
         n n]
    (if-not (zero? n)
      (recur next (.add curr next) (dec n))
      curr)))

; -----------------------------------------------

(defn run-bench*
  [name f n]
  (println "\n" name ": ------------------------------")
  (perf/bench (f n)))

(defmacro run-bench
  [[f n]]
  `(run-bench* ~(name f) ~f ~n))

(defn run-benches
  []
  (let [n 1000]
    (run-bench (fibo-iterate n))
    (run-bench (fibo-lazy-seq n))
    (run-bench (fibo-recur n))
    (run-bench (fibo-trampoline n))
    (run-bench (fibo-local-vars n))
    (run-bench (fibo-volatile n))
    (run-bench (fibo-with-type n))
    (run-bench (fibo-with-java n))
    (run-bench (fibo-recur-java-bigint n))
    #_(perf/quick-bench (fibo-lazy-cat n))
    ))
