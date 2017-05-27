(ns algorithms-clj.perf-fibo
  (:require
    [criterium.core :as perf]))

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

(defn fibo-iterate
  [n]
  (let [next-fib (fn [[a b]] [b (+ a b)])
        fibs (iterate next-fib [0N 1N])]
    (first (nth fibs n))))

(defn fibo-lazy-seq
  [n]
  (letfn [(fibs [a b] (cons a (lazy-seq (fibs b (+ a b)))))]
    (nth (fibs 0N 1N) n)))

(defn fibo-recur
  [n]
  (loop [curr 0N
         next 1N
         n n]
    (if-not (zero? n)
      (recur next (+ curr next) (dec n))
      curr)))

(defn fibo-trampoline
  [n]
  (letfn [(fibs [curr next n]
            (if-not (zero? n)
              #(fibs next (+ curr next) (dec n))
              curr))]
    (trampoline (fibs 0N 1N n))))

(defn fibo-local-vars
  [n]
  (with-local-vars [curr 0N
                    next 1N
                    iter n]
    (while (> @iter 0)
      (let [nnext (+ @curr @next)]
        (var-set curr @next)
        (var-set next nnext)
        (var-set iter (dec @iter))
        ))
    @curr))

(defn fibo-volatile
  [n]
  (let [curr (volatile! 0N)
        next (volatile! 1N)
        iter (volatile! n)]
    (while (> @iter 0)
      (let [nnext (+ @curr @next)]
        (vreset! curr @next)
        (vreset! next nnext)
        (vswap! iter dec)))
    @curr))

(defn fibo-lazy-cat
  ; TODO - This is weird... it should be a var, but then...
  [n]
  (letfn [(fibs [] (lazy-cat [0N 1N] (map + (rest (fibs)) (fibs))))]
    (nth (fibs) n)))

;; This gets really weird now...

(defprotocol Advance
  (advance [this n]))

(deftype FiboType [^:unsynchronized-mutable curr
                   ^:unsynchronized-mutable next]
  Advance
  (advance [_ n]
    (loop [n n]
      (if-not (zero? n)
        (let [nnext (+ curr next)]
          (set! curr next)
          (set! next nnext)
          (recur (dec n))
          )))
    curr))

(defn fibo-with-type
  [n]
  (advance (FiboType. 0N 1N) n))

; -----------------------------------------------

(defn run-bench*
  [name f n]
  (println name ": ------------------------------")
  (perf/quick-bench (f n)))

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
    #_(perf/quick-bench (fibo-lazy-cat n))
    ))
