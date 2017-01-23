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

(defn rator [e] (first e))
(defn rands [e] (rest e))

(defn cst? [n] (number? n))
(defn sym? [v] (string? v))
(defn op? [e] (vector? e))
(defn add? [e] (and (op? e) (= (rator e) :add)))
(defn mul? [e] (and (op? e) (= (rator e) :mul)))


;; ----------------------------------------------------------------------------
;; Test expression
;; ----------------------------------------------------------------------------

(def expr-1
  (add
    (cst 1) (cst 2)
    (mul (cst 0) (sym "x") (sym "y"))
    (mul (cst 1) (sym "y") (cst 2))
    (add (cst 0) (sym "x"))))

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

(defn evaluate
  [env e]
  (walk/postwalk
    (fn [e]
      (cond
        (sym? e) (get env e)
        (add? e) (reduce + (rest e))
        (mul? e) (reduce * (rest e))
        :else e))
    e))

;; ----------------------------------------------------------------------------

(defn- make-op
  [rator rands]
  (if (< 1 (count rands))
    (into [rator] rands)
    (first rands)))

(defn- optimize-op
  [[rator & rands] binary-op neutral]
  (let [{csts true vars false} (group-by cst? rands)
        sum-cst (reduce binary-op neutral csts)]
    (cond
      (empty? vars) (cst sum-cst)
      (= neutral sum-cst) (make-op rator vars)
      :else (make-op rator (into [(cst sum-cst)] vars))
      )))

(defn- optimize-add [e]
  (if (add? e)
    (optimize-op e + 0)
    e))

(defn- optimize-mul [e]
  (if (mul? e)
    (if (some #{(cst 0)} e)
      (cst 0)
      (optimize-op e * 1))
    e))

(defn optimize [e]
  (walk/postwalk (comp optimize-mul optimize-add) e))


;; ----------------------------------------------------------------------------

(defn replace-var
  [env x]
  (if (string? x) (get env x x) x))

(defn partial-eval
  [env e]
  (walk/postwalk
    (comp optimize-mul optimize-add #(replace-var env %))
    e))

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
;; A compiler (switch representation to lower level interpreter)
;; ----------------------------------------------------------------------------

(defn compile-op [e]
  (let [args (rands e)
        arg-count (count args)
        next-args (vec (apply concat args))]
    (conj next-args [(rator e) arg-count])))


(defn compile-expr
  "Compile an expression to a stack of instructions"
  [e]
  (walk/postwalk
    (fn [e]
      (cond
        (cst? e) [e]
        (sym? e) [e]
        (op? e) (compile-op e)
        :else e))
    e))

(defn apply-op
  "Stack transformer"
  [[op nb] stack]
  (let [consumed (take nb stack)
        remaining (drop nb stack)]
    (conj remaining
      (apply (if (= op :mul) * +) consumed)
      )))

(defn eval-bytecode
  "Evaluate compiled byte code"
  [env b]
  (loop [stack '()
         instr b]
    (if-let [i (first instr)]
      (recur
        (cond
          (cst? i) (conj stack i)
          (sym? i) (conj stack (get env i))
          (vector? i) (apply-op i stack))
        (rest instr))
      (peek stack))))

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
    (println (compile-expr e))
    (println (compile-expr o))
    (println (eval-bytecode env (compile-expr e)))
    (println (eval-bytecode env (compile-expr o)))
    ))
