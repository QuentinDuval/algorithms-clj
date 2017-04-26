(ns algorithms-clj.macro-initiation
  (:require
    [clojure.walk :as walk]
    ))


(defmacro expension-report
  "Debugging macro to show the result of the expension"
  [body]
  `(walk/macroexpand-all (quote ~body)))

(defmacro report
  [body]
  `(let [result# ~body]
     (println (expension-report ~body) "=>" result#)
     result#))

(defmacro constexpr
  [fct & args]
  (eval `(~fct ~@args)))

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

(defmacro add-m-3
  "Summing two integers known at compile time"
  [a b]
  ;; TODO - How to get rid of eval??? constexpr
  (eval `(+ ~a ~b)))

(defmacro add-m-4
  [a b]
  `(constexpr add ~a ~b))

(defn test-add
  []
  (let [x 1
        y 2]
    (report (add x y))
    (report (add-m 1 2))
    (report (add-m-2 1 2))
    (report (add-m-3 1 2))

    ;; Does not compile: cannot add symbols (explain this)
    ;; (println (add-m x y))
    ;; (println (add-m-2 x y))

    ;; This however works, but only with add-m-2
    (defmacro x1 [] 1)
    (defmacro x2 [] 2)
    ;; (println (add-m (x1) (x2)))
    (report (add-m-2 (x1) (x2)))
    (report (add-m-3 (x1) (x2)))
    (report (add-m-4 (x1) (x2)))
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
  `(constexpr average ~coll))

(defn test-average
  []
  (let [coll [1 2 3 4]]
    (report (average coll))
    ;; (println (average-m coll)) ;; Would not compile
    (report (average-m [1 2 3 4]))

    ;; This however works
    (defmacro coll-m [] [1 2 3 4])
    (report (average-m (coll-m)))
    ))


;; --------------------------------------------------------
;; Example 2-b: Compute frequency map at compile time
;; - Ok, collections, but also maps
;; - And there you are in troube with C++ constexpr
;; - And that's the thing: 1 language to learn, not 2
;; --------------------------------------------------------

(defn freq-map
  [coll]
  (reduce
    (fn [freqs val] (update freqs val (fnil + 0) 1))
    {}
    coll))

(defmacro freq-map-m
  [coll]
  `(constexpr freq-map ~coll))

(defn test-freq-map
  []
  (let [inputs [1 2 1 4 1 3]]
    (report (freq-map inputs))
    (report (freq-map-m [1 2 1 4 1 3]))

    (defmacro inputs-m [] [1 2 1 4 1 3])
    (report (freq-map-m (inputs-m)))
    ))


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
;; - This time, we add the argument in the call
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

(defmacro defn-log
  [name bindings body]
  `(defn ~name
     ~bindings
     (println "Entering the function with args:" ~@bindings)
     ~body))

(defn-log add-log
  [a b]
  (+ a b))

(defn test-add-log
  []
  (add-log 1 2))

;; --------------------------------------------------------
;; Example 4-b: Adding logs based on run time option (no overhead)
;; --------------------------------------------------------

(def default-logger (partial println "INFO:"))
(def logger (atom default-logger))

(defmacro defn-log-2
  [name bindings body]
  `(defn ~name
     ~bindings
     (if-let [log-fct# @logger]
       (log-fct# "Entering the function with args:" ~@bindings))
     ~body))

(defn-log-2 add-log-2
  [a b]
  (+ a b))

(defn test-add-log-2
  []
  (reset! logger default-logger)
  (println (add-log-2 1 2))
  (reset! logger nil)
  (println (add-log-2 1 2))
  (reset! logger default-logger))

;; --------------------------------------------------------
;; Example 4-c: Adding logs based on compile time option (no overhead)
;; --------------------------------------------------------

(defn logger-on []
  ;; The thing here, is that we could read a config file
  false)

(defmacro defn-log-3
  [name bindings body]
  (if (logger-on)
    `(defn ~name
       ~bindings
       (if-let [log-fct# @logger]
         (log-fct# "Entering the function with args:" ~@bindings))
       ~body)
    `(defn ~name
       ~bindings
       ~body)))

(defn-log-3 add-log-3
  [a b]
  (+ a b))

(defn test-add-log-3
  []
  (reset! logger default-logger)
  (println (add-log-3 1 2)))

;; --------------------------------------------------------
;; Example 5: Generating some code based on data structure
;; - Go iteratively?
;; - Show the example of efficient loop generation
;; --------------------------------------------------------

;; Phase 1
#_(defmacro inline-reduce
    [reducer initial transforms coll]
    `(reduce ~reducer ~initial ~coll))

;; Phase 2
#_(defmacro inline-reduce
    [reducer initial transforms coll]
    `(loop [h# (first ~coll)
            t# (rest ~coll)
            r# ~initial]
       (if h#
         (recur (first t#) (rest t#) (~reducer r# h#))
         r#)))

;; Phase 3
#_(defmacro inline-reducer
    [reducer transforms r h]
    `(~reducer ~r ~h))

;; Phase 4 (finished!)
(defmacro inline-reducer
  [reducer transforms r h]
  (let [[op fct] (first transforms)
        remaining (rest transforms)]
    (case op
      ;;TODO - Multimethod here for extensibility
      :map
      `(let [h2# (~fct ~h)]
         (inline-reducer ~reducer ~remaining ~r h2#))
      :filter
      `(if (~fct ~h)
         (inline-reducer ~reducer ~remaining ~r ~h)
         ~r)
      `(~reducer ~r ~h))))

(defmacro inline-reduce
  [reducer initial transforms coll]
  `(loop [h# (first ~coll)
          t# (rest ~coll)
          r# ~initial]
     (if h#
       (recur
         (first t#) (rest t#)
         (inline-reducer ~reducer ~transforms r# h#))
       r#)))

(defn test-inline-reduce
  []
  (let [coll (into [] (range 10))]
    (println (reduce + 0 (map #(* 2 %) (filter odd? coll))))

    (report (inline-reducer
              +
              [[:map #(* 2 %)]]
              5
              1))

    (report (inline-reducer
              +
              [[:filter odd?] [:map #(* 2 %)]]
              5
              1))

    (report (inline-reduce
              + 0
              [[:filter odd?] [:map #(* 2 %)]]
              coll))
    ))


;; --------------------------------------------------------
;; Example 6: DFS to do a topological sort
;; - You list a bunch of dependencies
;; - You get a list of tasks to run
;; Result:
;; 1. Simple (re-use topological sort runtime)
;; 2. Explicit ordering (not implicit positioning)
;; --------------------------------------------------------

;; TODO


;; --------------------------------------------------------
;; Example 7: Generating some code based on data structure
;; Something based on description of data model
;; - Generate the code to save it
;; - Generate the classes
;; - Find something else...
;; --------------------------------------------------------

;; TODO
