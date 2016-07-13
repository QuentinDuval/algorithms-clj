(ns algorithms-clj.derivative)



;; ---------------------------------------------------------------

(defn rotations
  "Generates all the rotations of an input sequence"
  [inputs]
  (let [n (count inputs)
        xf (comp (map #(take n %)) (take n))]
    (eduction xf
      (iterate #(drop 1 %) (concat inputs inputs)))
    ))

;; ---------------------------------------------------------------

(defn prod?
  [expr]
  (or
    (= (first expr) '*) ;; Works with simple quote
    (= (first expr) `*) ;; Works with syntax quote
    ))

(defn sum?
  [expr]
  (or
    (= (first expr) '+) ;; Works with simple quote
    (= (first expr) `+) ;; Works with syntax quote
    ))

(defn make-sum
  "Simplify the sum, adding the constants together"
  [terms]
  (let [parts (group-by number? terms)
        number-sum (reduce + (parts true))]
    (cond
      (empty? (parts true)) (list* '+ (parts false))
      (empty? (parts false)) number-sum
      :else (list* '+ number-sum (parts false))
      )))

(defn make-product
  "Simplify the product, multiplying the constants together"
  [terms]
  (let [parts (group-by number? terms)
        product (reduce * (parts true))]
    (cond
      (empty? (parts true)) (list* '* (parts false))
      (empty? (parts false)) product
      (zero? product) 0
      :else (list* '* product (parts false))
      )))

(declare derivative)

(defn derivative-prod
  [terms var]
  (for [r (rotations terms)]
    (make-product (conj (rest r) (derivative (first r) var)))
    ))

(defn derivative
  "Compute the partial derivative of the provided expression"
  [expr var]
  (cond
    (number? expr) 0
    (symbol? expr) (if (= expr var) 1 0)
    (sum? expr) (make-sum (map  #(derivative % var) (rest expr)))
    (prod? expr) (make-sum (derivative-prod (rest expr) var))
    ))

;; ---------------------------------------------------------------

(def x 5)
(def y 2)

(defn tests
  "Some basics tests (for the REPL)"
  []
  (prn (derivative 'x 'x))
  (prn (derivative 'x 'y))
  (prn (derivative `x `x))
  (println "------------------")
  (prn (make-sum ['x 'y]))
  (prn (make-sum [1 2 3]))
  (prn (make-sum [1 0 'x 5 'y]))
  (println "------------------")
  (prn (make-product ['x 'y]))
  (prn (make-product [1 'x 0]))
  (prn (make-product [2 'y 'x 5]))
  (println "------------------")
  (prn (derivative (list '+ 'x 1) 'x))
  (prn (derivative `(+ x 1) `x))
  (prn (derivative `(* x 2) `x))
  (println "------------------")
  (prn (derivative `(+ x (* x x y)) `x))
  )

