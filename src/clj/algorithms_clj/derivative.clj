(ns algorithms-clj.derivative
  (:require
    [algorithms-clj.utils :as utils]
    ))


;; ---------------------------------------------------------------

(defn prod? [expr] (contains? #{* '* `*} (first expr)))
(defn sum?  [expr] (contains? #{+ '+ `+} (first expr)))

(defn make-sum
  "Simplify the sum, adding the constants together"
  [terms]
  (let [parts (group-by number? terms)
        number-sum (reduce + (parts true))]
    (cond
      (empty? (parts false)) number-sum
      (zero? number-sum) (if-not (= 1 (count (parts false)))
                           (into [+] (parts false))
                           (first (parts false)))
      :else (into [+ number-sum] (parts false))
      )))

(defn make-product
  "Simplify the product, multiplying the constants together"
  [terms]
  (let [parts (group-by number? terms)
        product (reduce * (parts true))]
    (cond
      (empty? (parts false)) product
      (= 1 product) (if-not (= 1 (count (parts false)))
       (into [*] (parts false))
       (first (parts false)))
      (zero? product) 0
      :else (into [* product] (parts false))
      )))

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

