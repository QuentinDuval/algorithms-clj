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

(defmulti derivate-expr
  (fn [operand terms var] operand))

(defn derivative
  "Compute the partial derivative of the provided expression"
  [expr var]
  (cond
    (number? expr) 0
    (symbol? expr) (if (= expr var) 1 0)
    (keyword? expr) (if (= expr var) 1 0)
    :else (derivate-expr (first expr) (rest expr) var)
    ))

;; TODO: this is not a dispatch on symbol nor var, but on the content of the var
(defmethod derivate-expr +
  [_ terms var]
  (make-sum (map #(derivative % var) terms)))

;; TODO: this is not a dispatch on symbol nor var, but on the content of the var
(defmethod derivate-expr *
  [_ terms var]
  (make-sum
    (map
      (fn [[t & ts]] (make-product (conj ts (derivative t var))))
      (utils/rotations terms))
    ))


;; ---------------------------------------------------------------
;; Experiments: automatic quote of an expression
;; ---------------------------------------------------------------

(defn qualified-name [x]
  (symbol (str (.. (resolve x) -ns -name)) (name x)))

(defn var->symbol [x]
  (list 'quote (qualified-name x)))

(defn eq* [expr]
  (cond
    (vector? expr) (mapv eq* expr)
    (symbol? expr) (var->symbol expr)
    :else expr))

(defmacro eq [& expr]
  (first (eq* (vec expr))))

;; (derivative (eq [* 'x 'y]) 'x)

(defmacro eq2 [x]
  (list 'quote x))

;; (derivative (eq2 [* x y]) 'x)
