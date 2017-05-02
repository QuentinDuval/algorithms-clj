(ns algorithms-clj.print-macro)

(defmacro print-macro
  [code]
  (println "Compiling:" code)
  code)

(defn test-print-macro
  []
  (print-macro (+ 1 2)))
