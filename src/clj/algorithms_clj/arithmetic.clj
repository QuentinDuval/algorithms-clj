(ns algorithms-clj.arithmetic
  (:require
    [clojure.walk :as walk]
    ))


;; ----------------------------------------------------------------------------

(defn- optimize-form
  [form]
  (let [[rator & rands] form]
    (case rator
      :add
      (let [new-rands (filter (complement #{0}) rands)]
        (cond
          (= 1 (count new-rands)) (first new-rands)
          :else (into [:add] new-rands)))
      :mul
      (let [new-rands (filter (complement #{1}) rands)]
        (cond
          (some #{0} rands) 0
          (= 1 (count new-rands)) (first new-rands)
          :else (into [:mul] new-rands)))
      form)))

(defn optimize-cata
  [form]
  (cond
    (vector? form) (optimize-form form)
    :else form))

;; ----------------------------------------------------------------------------

(defn eval-op
  [env [rator & rands]]
  (case rator
    :add (reduce + rands)
    :mul (reduce * rands)
    ))

(defn eval-cata
  [env x]
  (cond
    (string? x) (get env x)
    (vector? x) (eval-op env x)
    :else x))

(defn eval-expr
  [env expr]
  (walk/postwalk (partial eval-cata env) expr))

;; ----------------------------------------------------------------------------

(defn fixing-cata
  [env x]
  (if (string? x) (get env x x) x))

(defn fixing
  [env expr]
  (walk/postwalk
    (comp optimize-cata (partial fixing-cata env))               ;; Cata-morphism composition
    expr))

;; ----------------------------------------------------------------------------

(defn test-walk
  []
  (let [expr [:add 1 2 [:mul "x" 0] [:mul 1 "y"] 0]
        opt (walk/postwalk (fn [x] (optimize-cata x)) expr)
        env {"x" 1 "y" 2}]
    (prn expr)
    (prn opt)
    (println "-------------")
    (prn (eval-expr env expr))
    (prn (eval-expr env opt))
    (prn (fixing {"y" 2} expr))
    ))
