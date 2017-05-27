(ns algorithms-clj.perf-fibo
  (:require
    [criterium.core :as perf]))


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
         iter n]
    (if-not (zero? iter)
      (recur next (+ curr next) (dec iter))
      curr)))

(defn fibo-imperative
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
        (vswap! iter dec)
        ))
    @curr))

(defn fibo-lazy-cat
  ; TODO - This is weird... it should be a var, but then...
  [n]
  (letfn [(fibs [] (lazy-cat [0N 1N] (map + (rest (fibs)) (fibs))))]
    (nth (fibs) n)))

(defn run-benches
  []
  (let [n 1000]
    (perf/quick-bench (fibo-iterate n))
    (perf/quick-bench (fibo-lazy-seq n))
    (perf/quick-bench (fibo-recur n))
    (perf/quick-bench (fibo-imperative n))
    #_(perf/quick-bench (fibo-lazy-cat n))
    ))
