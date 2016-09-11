(ns algorithms-clj.derivative
  (:require
    [algorithms-clj.utils :as utils]
    ))


;; ---------------------------------------------------------------
;; Constructors
;; ---------------------------------------------------------------

(defn- simplify-if-unary-op
  [expr]
  (cond
    (= 1 (count expr)) (apply (first expr))
    (= 2 (count expr)) (second expr)
    :else expr))

(defn make-sum
  "Simplify the sum, adding the constants together"
  [terms]
  (let [exprs (filter (complement number?) terms)
        number-sum (reduce + (filter number? terms))]
    (cond
      (empty? exprs) number-sum
      (zero? number-sum) (simplify-if-unary-op (into [+] exprs))
      :else (into [+ number-sum] exprs)
      )))

(defn make-product
  "Simplify the product, multiplying the constants together"
  [terms]
  (let [exprs (filter (complement number?) terms)
        product (reduce * (filter number? terms))]
    (cond
      (empty? exprs) product
      (zero? product) 0
      (= 1 product) (simplify-if-unary-op (into [*] exprs))
      :else (into [* product] exprs)
      )))


;; ---------------------------------------------------------------
;; Derivative of expression 
;; ---------------------------------------------------------------

(defn prod? [expr] (contains? #{* '* `*} (first expr)))
(defn sum?  [expr] (contains? #{+ '+ `+} (first expr)))

(declare derivative)

(defn derivative-prod
  [terms var]
  (for [r (utils/rotations terms)]
    (make-product (conj (rest r) (derivative (first r) var)))
    ))

(defn derivative
  "Compute the partial derivative of the provided expression"
  [expr var]
  (cond
    (number? expr) 0
    (symbol? expr) (if (= expr var) 1 0)
    (keyword? expr) (if (= expr var) 1 0)
    (sum? expr) (make-sum (map  #(derivative % var) (rest expr)))
    (prod? expr) (make-sum (derivative-prod (rest expr) var))
    ))

