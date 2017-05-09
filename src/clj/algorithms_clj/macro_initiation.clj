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

#_(defmacro constexpr-2
    [[fct & args]]
    (list* fct (map (fn [a] `~a) args)))

#_(defmacro defconstexpr
    [name arguments body]
    (println (str body))
    `(defmacro ~name ~arguments
       (constexpr ~body)))

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

(defmacro add-inline
  "Inlining the sum of two integers"
  [a b]
  `(+ ~a ~b))

(defmacro add-m-3
  "Summing two integers known at compile time"
  [a b]
  (eval `(+ ~a ~b)))

(defmacro add-m-4
  "Summing two integers known at compile time"
  [a b]
  `(constexpr add ~a ~b))

(defmacro sum-m
  []
  (add 1 2))

(def ^:const c1 1)
(def ^:const c2 2)
(def ^:const c3 (add c1 c2))
(def ^:const c4 (add-m 1 2))

(def x1 1)
(def x2 2)

(defn test-add
  []
  (let [x 1
        y 2]
    (report (add x y))
    (report (add-m 1 2))
    (report (add-inline 1 2))
    (report (add-m-3 1 2))

    ;; Does not compile: cannot add symbols (explain this)
    ;; (println (add-m x y))
    ;; (println (add-m-2 x y))

    ;; This however works, thanks to eval or custom macro
    (report (add-inline x y))
    (report (add-m-3 x1 x2))
    (report (add-m-4 x1 x2))
    (report (sum-m))

    ;; Up to the JVM to optimize it
    (report c3)
    (report c4)
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

(defmacro my-average
  []
  (average-m [1 2 3 4]))

(def ^:const my-average-2
  (average-m [1 2 3 4]))

(defn test-average
  []
  (let [coll [1 2 3 4]]
    (report (average coll))
    ;; (println (average-m coll)) ;; Would not compile
    (report (average-m [1 2 3 4]))
    (report (my-average))
    (report my-average-2)
    ))


;; --------------------------------------------------------
;; Example 2-b: Compute frequency map at compile time
;; - Ok, collections, but also maps
;; - And there you are in troube with C++ constexpr
;; - And that's the thing: 1 language to learn, not 2
;; --------------------------------------------------------

#_(set! *warn-on-reflection* true)

(defn freq-map-opt
  "Optimized version of the frequency map"
  [coll]
  (let [freqs (java.util.HashMap.)]
    (doseq [val coll]
      (.put freqs val (inc (get freqs val 0))))
    freqs))

(defn freq-map
  [coll]
  (persistent!
    (reduce
      (fn [freqs val]
        (assoc! freqs val (inc (get freqs val 0))))
      (transient {})
      coll)))

(defmacro freq-map-m
  [coll]
  `(constexpr freq-map ~coll))

(defmacro inputs-m [] [1 2 1 4 1 3])
(definline inputs-m2 [] [1 2 1 4 1 3])

(defn test-freq-map
  []
  (let [inputs [1 2 1 4 1 3]]
    (report (freq-map inputs))
    (report (freq-map-m [1 2 1 4 1 3]))

    (report (freq-map-m (inputs-m)))
    (report (freq-map-m (inputs-m2)))
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
;; Example 4: Generating some code based on data structure
;; - Go iteratively?
;; - Show the example of efficient loop generation
;; --------------------------------------------------------

;; Note: how to do recursive nested loops:
#_(loop [i 5
         r 0]
    (if (pos? i)
      (recur
        (dec i)
        (loop [j 5
               r r]
          (if (pos? j)
            (recur (dec j) (inc r))
            r)
          ))
      r))

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
      :map
      `(let [h2# (~fct ~h)]
         (inline-reducer ~reducer ~remaining ~r h2#))
      :filter
      `(if (~fct ~h)
         (inline-reducer ~reducer ~remaining ~r ~h)
         ~r)
      :mapcat
      (let [h2 (with-meta (gensym "h2") {:tag 'java.lang.Iterable})]
        `(let [~h2 (~fct ~h)]
           (inline-reduce ~reducer ~r ~remaining ~h2)))
      `(~reducer ~r ~h))))

(defmacro inline-reduce
  [reducer initial transforms coll]
  (let [iter (with-meta (gensym "iter") {:tag 'java.util.Iterator})]
    `(let [~iter (.iterator ~coll)]
       (loop [r# ~initial]
         (if (.hasNext ~iter)
           (let [h# (.next ~iter)
                 r2# (inline-reducer ~reducer ~transforms r# h#)]
             (recur r2#))
           r#)))))

(defn test-inline-reduce
  ;; TODO - We could do better and accept a form, not a lambda
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

    (report (inline-reduce
              + 0
              [[:filter odd?]
               [:map #(* 2 %)]
               [:mapcat (fn [x] [x x])]]
              coll))
    ))

;; Versus reducer API

(defn filterer [pred next]
  (fn [result val]
    (if (pred val)
      (next result val)
      result)))

(defn mapper [xf next]
  (fn [result val]
    (next result (xf val))))

(defn map-catter [xf next]
  (fn [result val]
    (reduce next result (xf val))))

(defn test-reducers
  []
  (let [coll (into [] (range 5000))
        repeat-2 (fn [x] [x x])]

    (println "Naive reduction: 10ms")
    (time
      (dotimes [i 10]
        (reduce + 0
          (mapcat repeat-2 (map #(* 2 %) (filter odd? coll)))
          )))

    (println "With reducers: 2ms")
    (time
      (dotimes [i 10]
        (reduce
          (filterer odd? (mapper #(* 2 %) (map-catter repeat-2 +)))
          0 coll)))

    (println "With transducers: 2ms")
    (time
      (dotimes [i 10]
        (transduce
          (comp
            (filter odd?)
            (map #(* 2 %))
            (mapcat repeat-2))
          + 0 coll)))

    (println "With inline-reduce: 1.4ms")
    (time
      (dotimes [i 10]
        (inline-reduce
          + 0
          [[:filter odd?]
           [:map #(* 2 %)]
           [:mapcat repeat-2]]
          coll)))
    ))


;; --------------------------------------------------------
;; Example 5-a: Adding logs around functions
;;
;; Show that we can extend the language as desired
;; Macros can help address operational constraints
;; --------------------------------------------------------

(defn log-enter-message
  "Returns the log message for the function and its arguments"
  [log-prefix arg-names arg-values]
  (str log-prefix (zipmap arg-names arg-values)))

(defn bindings->bound-vars
  "Clean the bindings to remove the restructuring artifacts"
  [bindings]
  (->>
    (flatten bindings)
    (remove #{'& '_})
    (remove keyword?)
    (vec)))

(defn log-prefix
  [fct-name]
  (str "[TRACE] Entering \"" fct-name "\" with arguments: "))

(defn compile-log-message
  [form binding-vec]
  (let [fct-name (-> form second str)
        bindings (bindings->bound-vars binding-vec)]
    `(log-enter-message
       ~(log-prefix fct-name)
       (quote ~bindings)
       ~bindings)))

(defmacro defn-log
  [name bindings body]
  `(defn ~name
     ~bindings
     (println ~(compile-log-message &form bindings))
     ~body))

(defn-log add-with-log
  [a b]
  (+ a b))

(defn test-add-with-log
  []
  (println
    (walk/macroexpand-all
      '(defn-log add-with-log [a b] (+ a b))))
  (add-with-log 1 2))

;; --------------------------------------------------------
;; Example 5-b: Adding logs based on run time option (no overhead)
;; --------------------------------------------------------

(def default-logger (partial println "INFO:"))
(def logger (atom default-logger))

(defmacro defn-log-2
  [name bindings body]
  `(defn ~name
     ~bindings
     (if-let [log-fct# @logger]
       (log-fct# ~(compile-log-message &form bindings)))
     ~body))

(defn-log-2 add-with-log-2
  [a b]
  (+ a b))

(defn test-add-with-log-2
  []
  (reset! logger default-logger)
  (println (add-with-log-2 1 2))
  (reset! logger nil)
  (println (add-with-log-2 1 2))
  (reset! logger default-logger))

;; --------------------------------------------------------
;; Example 5-c: Adding logs based on compile time option (no overhead)
;; --------------------------------------------------------

(def ^:const log-config-file
  "./src/clj/algorithms_clj/macro_initiation_resource.edn")

(defn logger-enabled?
  "Read whether the log should be enabled by reading config file"
  []
  (let [log-config (read-string (slurp log-config-file))]
    (-> log-config :log :enabled?)))

(defmacro defn-log-3
  [name bindings body]
  (if (logger-enabled?)
    `(defn-log-2 ~name
       ~bindings
       ~body)
    `(defn ~name
       ~bindings
       ~body)))

(defn-log-3 add-with-log-3
  [a b]
  (+ a b))

(defn-log-3 get-first
  [[x & _ :as list]]
  x)

(defn test-add-with-log-3
  []
  (reset! logger default-logger)
  (println (add-with-log-3 1 2))
  (println (get-first [1 2 3])))


;; --------------------------------------------------------
;; Example 6: DFS to do a topological sort
;; - You list a bunch of dependencies
;; - You get a list of tasks to run
;;
;; Show that we can use macro for software architecture
;;
;; Result:
;; 1. Simple (re-use topological sort runtime)
;; 2. Explicit ordering (not implicit positioning)
;; --------------------------------------------------------

(def modules
  {:rest-api {:prerequisites [:trade-db :logger :monitor]
              :init-sequence [println "rest ai init"]
              :shut-sequence [println "rest ai shut"]}
   :trade-db {:prerequisites [:monitor]
              :init-sequence [println "trade db init"]
              :shut-sequence [println "trade db shut"]}
   :monitor {:prerequisites []
             :init-sequence [println "monitor init"]
             :shut-sequence [println "monitor shut"]}
   :logger {:prerequisites [:monitor]
            :init-sequence [println "logger init"]
            :shut-sequence [println "logger shut"]}})

(defn modules->dependency-graph
  [modules]
  (reduce-kv
    (fn [deps k v] (assoc deps k (:prerequisites v)))
    {}
    modules))

(def module-dependencies (modules->dependency-graph modules))

(defn post-order-visit
  [{:keys [graph not-visited sorted] :as dfs} node]
  (if (not-visited node)
    (->
      (reduce post-order-visit
        (update dfs :not-visited disj node)
        (get graph node []))
      (update :sorted conj node))
    dfs))

(defn topological-sort
  [graph]
  (some
    #(if (-> % :not-visited empty?) (:sorted %))
    (iterate
      #(post-order-visit % (-> % :not-visited first))
      {:graph graph
       :not-visited (set (keys graph))
       :sorted []})))

(defn test-topological-sort
  []
  (println (topological-sort {:a [:b] :b [:c] :c []}))
  (println (topological-sort module-dependencies))
  )

(defn modules->init-calls
  [modules]
  (let [dependencies (modules->dependency-graph modules)
        ordered-seq (topological-sort dependencies)
        ordered-init (map #(get-in modules [% :init-sequence]) ordered-seq)]
    (map (fn [[f & args]] `(~f ~@args)) ordered-init)))

(defmacro compile-init-sequence
  [modules]
  (let [ordered-calls (modules->init-calls (eval modules))]
    `{:init (fn [] (do ~@ordered-calls))
      :shut (fn [] (do ~@(reverse ordered-calls)))
      }))

(def init-shut-sequence (compile-init-sequence modules))

(defn run-init-sequence
  []
  ((:init init-shut-sequence)))

(defn run-shut-sequence
  []
  ((:shut init-shut-sequence)))


;; --------------------------------------------------------
;; Example 7: Macros can be used at the requirement level:
;; - Expression your business
;; - Get powerful code for it
;;
;; Phase 1: Factorize some common used patterns
;; - tree transformation of collect dependencies
;; - tree transformation of evaluate with env
;; => Factorize into tree morphism
;;
;; Phase 2: Generating some code based on data structure
;;
;; Here we define a computation in terms of variables
;; - We produce a record for this computation (type)
;; - We produce an implementation of an evaluator
;; - Everything is done at compile time (and can be tested)
;; --------------------------------------------------------

(def computation-tree [+ :a [* :b :c]])

(defn keyword->symbol [k] (-> k name symbol))

#_(defn collect-dependencies
    "Collect all the dependencies of an environment"
    [tree]
    (walk/postwalk
      (fn [node]
        (cond
          (keyword? node) #{node}
          (vector? node) (apply clojure.set/union node)
          :else #{}))
      tree))

#_(defn eval-expr-in-env
    "Evaluate the expression in an environment"
    [tree env]
    (walk/postwalk
      (fn [node]
        (cond
          (keyword? node) (get env node)
          (vector? node) (apply (first node) (rest node))
          :else node))
      tree))

(defmacro tree-catamorph
  [[tree-symbol node-symbol] & conditions]
  `(walk/postwalk
     (fn [~node-symbol] (cond ~@conditions))
     ~tree-symbol))

(defn collect-dependencies
  "Collect all the dependencies of an environment"
  [tree]
  (tree-catamorph [tree node]
    (keyword? node) #{node}
    (vector? node) (apply clojure.set/union node)
    :else #{}))

(defn eval-expr-in-env
  "Evaluate the expression in an environment"
  [tree env]
  (tree-catamorph [tree node]
    (keyword? node) (get env node)
    (vector? node) (apply (first node) (rest node))
    :else node))

(defn resolve-operators
  [tree]
  (tree-catamorph [tree node]
    (symbol? node) (resolve node)
    :else node))

(defn optimize-op
  [[op & args :as expr]]
  (let [variables (filter (complement number?) args)
        constants (filter number? args)
        reduced (reduce op constants)]
    (if-not (empty? constants)
      (into [op reduced] variables)
      expr)))

(defn optimize-expr
  "Optimize the shape of an expression"
  [tree]
  (tree-catamorph [tree node]
    (vector? node) (optimize-op node)
    :else node))

;; Phase 2:
;; Optimizing and creating types for commonly used expressions.
;; - We can compile the expression
;; - We can optimize it first
;; Explain the link with expression templates

(defn compile-eval-expr
  "Compile the expression in optimal code"
  [tree symbol-env]
  (tree-catamorph [tree node]
    (keyword? node) `(~node ~symbol-env)
    (vector? node) `(~(first node) ~@(rest node))
    :else node))

(defprotocol IEvalExpr
  (eval-expr [this] "Evaluate the expression")
  (expr->data [this] "The form used to create it"))

(defmacro def-expr
  [name tree]
  (let [deps (sort (map keyword->symbol (collect-dependencies tree)))
        fast (optimize-expr (resolve-operators tree))
        this (gensym "this")]
    `(defrecord ~name ~(vec deps)
       IEvalExpr
       ;; (eval-expr [~this] (eval-expr-in-env ~tree ~this))
       (eval-expr [~this] ~(compile-eval-expr fast this))
       (expr->data [_] (quote ~tree))
       )))

(def-expr Expr [+ [* 2 3 :a] [* 2 :b :c]])

(defn test-def-expr
  []
  (let [e (map->Expr {:a 1 :b 2 :c 3})]
    (println (eval-expr e))
    (println (expr->data e))
    ))
