(ns algorithms-clj.print-macro)

(defmacro print-macro
  [code]
  (println "Compiling:" code)
  code)

(defn add-two
  [a b]
  (print-macro (+ a b)))

(defn test-print-macro
  []
  (add-two 1 2))
