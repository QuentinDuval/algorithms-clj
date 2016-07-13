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
    (= (first expr) `*) ;; Works with ` quote
    ))

(defn sum?
  [expr]
  (or
    (= (first expr) '+) ;; Works with simple quote
    (= (first expr) `+) ;; Works with ` quote
    ))

(declare derivative)

(defn derivative-prod
  [terms var]
  (for [r (rotations terms)]
    (list* '* (derivative (first r) var) (rest r))
    ))

(defn derivative
  "Compute the partial derivative of the provided expression"
  [expr var]
  (cond
    (number? expr) 0
    (symbol? expr) (if (= expr var) 1 0)
    (sum? expr) (list* '+ (map  #(derivative % var) (rest expr)))
    (prod? expr) (list* '+ (derivative-prod (rest expr) var))
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
  
  (prn (derivative (list '+ 'x 1) 'x))
  (prn (derivative `(+ x 1) `x))
  (prn (derivative `(* x 2) `x))
  )

