(ns algorithms-clj.macro-initiation)


;; --------------------------------------------------------
;; Example 1: equivalent with constexpr
;; --------------------------------------------------------

(defn add
  "Summing two integers known at runtime"
  [a b]
  (+ a b))

(defmacro add-m
  "Summing two integers known at compile time"
  [a b]
  (+ a b))

(defmacro add-m-2
  "Summing two integers known at compile time"
  [a b]
  `(+ ~a ~b))

(defn test-add
  []
  (let [x 1
        y 2]
    (println (add x y))
    (println (add-m 1 2))
    (println (add-m-2 1 2))

    ;; Does not compile: cannot add symbols (explain this)
    ;; (println (add-m x y))
    ;; (println (add-m-2 x y))

    ;; This however works, but only with add-m-2
    (defmacro x1 [] 1)
    (defmacro x2 [] 2)
    ;; (println (add-m (x1) (x2)))
    (println (add-m-2 (x1) (x2)))
    ))


;; --------------------------------------------------------
;; Example 2-a: average at compile time
;; - Show how we can do it in C++ with fold expressions C++17
;; --------------------------------------------------------

(defn average
  "Average of numbers known at runtime"
  [coll]
  (/ (reduce + coll) (count coll)))

(defmacro average-m
  "Average of numbers known at compile time"
  [coll]
  `(average ~coll))

(defn test-average
  []
  (let [coll [1 2 3]]
    (println (average coll))
    ;; (println (average-m coll)) ;; Would not compile
    (println (average-m [1 2 3]))

    ;; This however works
    (defmacro coll-m [] [1 2 3])
    (println (average-m (coll-m)))
    ))


;; --------------------------------------------------------
;; Example 2-b: Compute frequency map at compile time
;;
;; TODO - Ok, collections, but also maps
;; And there you are in troube with C++ constexpr
;; And that's the thing: 1 language to learn, not 2
;; --------------------------------------------------------

;; TODO


;; --------------------------------------------------------
;; Example 3-a: repeat a side-effectful call 3 times
;; --------------------------------------------------------

(defmacro times-3
  [body]
  `(do
     ~body
     ~body
     ~body))

(defmacro times-3-2
  [body]
  (let [bodies (repeat 3 body)]
    `(do ~@bodies)))

(defn print-3 [] (times-3 (println "1")))
(defn print-3-2 [] (times-3-2 (println "1")))


;; --------------------------------------------------------
;; Example 3-b: repeat a side-effectful call N times
;; --------------------------------------------------------

(defmacro times
  [n body]
  (let [bodies (for [i (range n)] (list body i))]
    `(do ~@bodies)))

(defn time-n [] (times 3 println))


;; --------------------------------------------------------
;; Example 3-c: unroll a side-effect loop by chunk of 4
;; --------------------------------------------------------

(defmacro unrolled-loop
  [n body]
  (let [bodies (repeat n body)
        chunks (partition 4 bodies)
        chunk-nb (count chunks)
        remaining (drop (* 4 chunk-nb) bodies)]
    `(do
       (dotimes [i# ~chunk-nb]
         (println (str "Chunk:" i#))
         ~@(first chunks))
       (println (str "Chunk:" ~chunk-nb))
       ~@remaining)))

(defn test-unroll-loop []
  ;; Use Ctrl-Alt-M (on Mac) to show full expand
  (unrolled-loop 10 (println "Yoh")))


;; --------------------------------------------------------
;; Example 4-a: Adding logs around functions
;; --------------------------------------------------------

;; TODO

;; --------------------------------------------------------
;; Example 4-b: Adding logs based on compile time option (no overhead)
;; --------------------------------------------------------

;; TODO

;; --------------------------------------------------------
;; Example 5: DFS to do a topological sort
;; - You list a bunch of dependencies
;; - You get a list of tasks to run
;; Result:
;; 1. Simple (re-use topological sort runtime)
;; 2. Explicit ordering (not implicit positioning)
;; --------------------------------------------------------

;; TODO

;; --------------------------------------------------------
;; Example 6: Generating some code based on data structure
;; --------------------------------------------------------

;; TODO
