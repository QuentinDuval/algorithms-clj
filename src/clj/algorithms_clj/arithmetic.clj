(ns algorithms-clj.arithmetic
  (:require
    [clojure.string :as string]
    [clojure.set :as set]
    [clojure.walk :as walk]
    ))


;; ----------------------------------------------------------------------------
;; DSL constructors
;; ----------------------------------------------------------------------------

(defn cst [n] n)
(defn sym [s] s)
(defn add [& args] (into [:add] args))
(defn mul [& args] (into [:mul] args))

(defn cst? [n] (number? n))
(defn sym? [v] (string? v))
(defn op? [e] (vector? e))
(defn add? [e] (and (op? e) (= (first e) :add)))
(defn mul? [e] (and (op? e) (= (first e) :mul)))

(defn rands [e] (rest e))


;; ----------------------------------------------------------------------------
;; Test expression
;; ----------------------------------------------------------------------------

(def expr-1
  (add
    (cst 1) (cst 2)
    (mul (cst 0) (sym "x") (sym "y"))
    (mul (cst 1) (sym "y") (cst 2))
    (add (cst 0) (sym "x"))))

(def expr-2
  (add 1 2
    (mul 0 "x" "y")
    (mul 1 "y" 2)
    (add 0 "y")))

;; ----------------------------------------------------------------------------

(defn print-expr
  [expr]
  (walk/postwalk
    (fn [e]
      (cond
        (cst? e) (str e)
        (sym? e) e
        (add? e) (str "(+ " (string/join " " (rest e)) ")")
        (mul? e) (str "(* " (string/join " " (rest e)) ")")
        :else e))
    expr))

;; ----------------------------------------------------------------------------

(defn eval-cata
  [env e]
  (cond
    (sym? e) (get env e)
    (add? e) (reduce + (rest e))
    (mul? e) (reduce * (rest e))
    :else e))

(defn evaluate
  [env expr]
  (walk/postwalk (partial eval-cata env) expr))

;; ----------------------------------------------------------------------------

(defn- optimize-op
  [[rator & rands] binary-op neutral]
  (let [groups (group-by cst? rands)
        csts (get groups true)
        vars (get groups false)
        sum-cst (reduce binary-op neutral csts)]
    (cond
      (empty? vars) (cst sum-cst)
      (and (= 1 (count vars)) (= sum-cst neutral)) (sym (first vars))
      (= sum-cst neutral) (into [rator] vars)
      :else (into [rator sum-cst] vars)
      )))

(defn- optimize-add [e]
  (cond
    (add? e) (optimize-op e + 0)
    :else e))

(defn- optimize-mul [e]
  (cond
    (mul? e) (if (some #{0} e) (cst 0) (optimize-op e * 1))
    :else e))

(defn optimize
  [e]
  (walk/postwalk
    (comp optimize-mul optimize-add)
    e))


;; ----------------------------------------------------------------------------

(defn replace-var
  [env x]
  (if (string? x) (get env x x) x))

(defn partial-eval
  [env expr]
  (walk/postwalk
    (comp optimize-mul optimize-add (partial replace-var env))
    expr))

;; ----------------------------------------------------------------------------

(defn dependencies [e]
  (walk/postwalk
    (fn [e]
      (cond
        (cst? e) #{}
        (sym? e) #{e}
        (op? e) (apply set/union (rands e))
        :else e))
    e))

;; ----------------------------------------------------------------------------

(defn test-walk
  []
  (let [e expr-1
        env {"x" 1 "y" 2}
        o (optimize e)
        f (partial-eval {"y" 0} e)]
    (println (print-expr e))
    (println (print-expr o))
    (println (print-expr f))
    (println (dependencies e))
    (println (dependencies f))
    (println (evaluate env e))
    (println (evaluate env o))
    ))
