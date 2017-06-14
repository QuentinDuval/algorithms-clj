(ns algorithms-clj.macro-initiation
  (:require
    [clojure.walk :as walk]
    [criterium.core :as perf]
    [clojure.test :as test :refer [deftest is are testing]]
    ))


; (set! *warn-on-reflection* true)
; (set! *unchecked-math* :warn-on-boxed)


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
(defmacro compile-reducer
  [reducer transforms r h]
  (let [[op fct] (first transforms)
        remaining (rest transforms)]
    (case op
      :map
      `(let [h2# (~fct ~h)]
         (compile-reducer ~reducer ~remaining ~r h2#))
      :filter
      `(if (~fct ~h)
         (compile-reducer ~reducer ~remaining ~r ~h)
         ~r)
      :mapcat
      (let [h2 (with-meta (gensym "h2") {:tag 'java.lang.Iterable})]
        `(let [~h2 (~fct ~h)]
           (inline-reduce ~reducer ~r ~remaining ~h2)))
      `(~reducer ~r ~h))))

(defmacro inline-reduce
  [reducer initial transforms coll]
  (let [iter (with-meta (gensym "iter") {:tag 'java.util.Iterator})
        coll (with-meta coll {:tag 'java.lang.Iterable})]
    `(let [~iter (.iterator ~coll)]
       (loop [r# ~initial]
         (if (.hasNext ~iter)
           (let [h# (.next ~iter)
                 r2# (compile-reducer ~reducer ~transforms r# h#)]
             (recur r2#))
           r#)))))

(defn inlined-reduce
  [coll]
  (let*
    [^java.util.Iterator iter33662 (. ^java.lang.Iterable coll iterator)]
    (loop*
      [r__33312__auto__ 0]
      (if
        (. iter33662 hasNext)
        (let*
          [h__33313__auto__  (. iter33662 next)
           r2__33314__auto__ (+ r__33312__auto__ h__33313__auto__)]
          (recur r2__33314__auto__))
        r__33312__auto__))))

(defn test-inline-reduce
  ;; TODO - We could do better and accept a form, not a lambda
  []
  (let [coll (vec (range 10))]

    (report (compile-reducer
              +
              [[:map #(* 2 %)]]
              5
              1))

    (report (compile-reducer
              +
              [[:filter odd?] [:map #(* 2 %)]]
              5
              1))

    (report (inline-reduce
              + 0
              []
              coll))

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


;; --------------------------------------------------------
;; Versus reducer API
;; --------------------------------------------------------

(defn run-bench*
  [name f n]
  (println "\n" name ": ------------------------------")
  (perf/quick-bench (f n)))

(defmacro run-bench
  [[f n]]
  `(run-bench* ~(name f) ~f ~n))

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

(defn bench-reducers
  []
  (let [coll (vec (range 5000))
        times-2 (fn [^long x] (* x 2))
        repeat-2 (fn [x] [x x])]

    (letfn [(naive-reduction [coll]
              (reduce + 0
                (mapcat repeat-2
                  (map times-2
                    (filter odd? coll)))))]
      (run-bench (naive-reduction coll)))

    (letfn [(with-reducers [coll]
              (reduce
                (filterer odd?
                  (mapper times-2
                    (map-catter repeat-2 +)))
                0 coll))]
      (run-bench (with-reducers coll)))

    (letfn [(with-transducer [coll]
              (transduce
                (comp
                  (filter odd?)
                  (map times-2)
                  (mapcat repeat-2))
                + 0 coll))]
      (run-bench (with-transducer coll)))

    (letfn [(with-inline-reduce [coll]
              (inline-reduce
                + 0
                [[:filter odd?]
                 [:map times-2]
                 [:mapcat repeat-2]]
                coll))]
      (run-bench (with-inline-reduce coll)))
    ))


;; --------------------------------------------------------
;; Example 5-a: Adding logs around functions
;;
;; Show that we can extend the language as desired
;; Macros can help address operational constraints
;; --------------------------------------------------------

(defn log-symbol-values
  "Log the values of a list of symbols, with a prefix"
  [prefix symbols values]
  (str prefix (zipmap symbols values)))

(defn bindings->bound-vars
  "Clean the bindings to remove the restructuring artifacts"
  [bindings]
  (->>
    (flatten bindings)
    (remove #{'& '_})
    (remove keyword?)
    (vec)))

(defn function-log-prefix
  "Return the prefix of the log entry for a function"
  [fct-name]
  (str "[TRACE] Call to \"" fct-name "\" with arguments: "))

(defn compile-log-message
  [fct-name binding-vec]
  (let [bindings (bindings->bound-vars binding-vec)]
    `(log-symbol-values
       ~(function-log-prefix fct-name)
       '~bindings
       ~bindings)))

(defmacro defn-log
  [name bindings & body]
  `(defn ~name
     ~bindings
     (println ~(compile-log-message name bindings))
     ~@body))

(defn-log add-with-log
  [a b]
  (+ a b))

(defn-log repeat-n
  [n s]
  (apply str (repeat n s)))

(defn repeat-n-with-logs
  [n s]
  (prn (log-symbol-values
         (function-log-prefix "repeat-n")
         '[n s] [n s]))
  (apply str (repeat n s)))

(defn test-add-with-log
  []
  (println
    (walk/macroexpand-all
      '(defn-log add-with-log [a b] (+ a b))))
  (add-with-log 1 2))

;; --------------------------------------------------------
;; Example 5-b: Adding logs based on run time option (no overhead)
;; --------------------------------------------------------

(def default-logger println)
(def logger (atom default-logger))

(defmacro defn-log-2
  [name bindings body]
  `(defn ~name
     ~bindings
     (if-let [log-fct# @logger]
       (log-fct# ~(compile-log-message name bindings)))
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
  (-> (slurp log-config-file) (read-string) :log :enabled?))

(defmacro defn-log-3
  [& args]
  (if (logger-enabled?)
    `(defn-log-2 ~@args)
    `(defn ~@args)))

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

; TODO - provide the argument map as input to the functions?
; TODO - In fact, just replace all occurence of :keyword by deref of the map (capture)

(defprotocol IModule
  (-init [this dependencies] "Start the module")
  (-shut [this dependencies] "Stop the module"))

(defrecord RestEnpoint []
  IModule
  (-init [this deps] (prn "Start rest: " deps) this)
  (-shut [this deps] (prn "Stop rest: " deps) this))

(defrecord TradeDb []
  IModule
  (-init [this deps] (prn "Start Trade DB: " deps) this)
  (-shut [this deps] (prn "Stop Trade DB: " deps) this))

(defrecord Monitor []
  IModule
  (-init [this deps] (prn "Start Monitor: " deps) this)
  (-shut [this deps] (prn "Stop Monitor: " deps) this))

(defrecord Logger []
  IModule
  (-init [this deps] (prn "Start Logger: " deps) this)
  (-shut [this deps] (prn "Stop Logger: " deps) this))

(def modules
  {:rest-api {:prerequisites [:trade-db :logger :monitor]
              :implementation (RestEnpoint.)}
   :trade-db {:prerequisites [:monitor]
              :implementation (TradeDb.)}
   :monitor {:prerequisites []
             :implementation (Monitor.)}
   :logger {:prerequisites [:monitor]
            :implementation (Logger.)}})

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
  ; TODO - make unit test
  []
  (println (topological-sort {:a [:b] :b [:c] :c []}))
  (println (topological-sort module-dependencies)))

(defn ordered-modules
  [modules]
  (let [dependencies (modules->dependency-graph modules)]
    (mapv
      (fn [module-id]
        [module-id (get-in modules [module-id :implementation])])
      (topological-sort dependencies))))

(defn init-and-register
  [dependencies module-id]
  (update dependencies module-id #(-init % dependencies)))

(defn init-all-modules
  [dependencies module-ids]
  (reduce init-and-register dependencies module-ids))

(defn shut-and-unregister
  [dependencies module-id]
  (update dependencies module-id #(-shut % dependencies)))

(defn shut-all-modules
  [dependencies module-ids]
  (reduce shut-and-unregister dependencies module-ids))

(defmacro compile-init-sequence
  [modules]
  (let [modules (ordered-modules (eval modules))
        init-order (vec (map first modules))
        shut-order (vec (reverse init-order))]
    `(let [system# (atom (into {} ~modules))]
       (defn init-all []
         (swap! system# init-all-modules ~init-order))
       (defn shut-all []
         (swap! system# shut-all-modules ~shut-order)
         ))))

(compile-init-sequence modules)


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

(defn keyword->symbol [k] (-> k name symbol))

; This are the plain ways to write the code

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

; Here is how you can greatly simplify it

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
        constants (filter number? args)]
    (if (< 1 (count constants))
      (into [op (apply op constants)] variables)
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
       ; This is the low part one (where we do at run-time)
       ; (eval-expr [~this] (eval-expr-in-env ~tree ~this))
       ; But you can go must faster by compiling
       (eval-expr [~this] ~(compile-eval-expr fast this))
       (expr->data [_] (quote ~tree))
       )))

(def-expr Expr [+ [* 2 3 :a] [* 2 :b :c]])

(deftest test-def-expr
  (let [e (map->Expr {:a 1 :b 2 :c 3})]
    (is (= 18 (eval-expr e)))
    (is (= '[+ [* 2 3 :a] [* 2 :b :c]] (expr->data e)))
    ))


;; Phase 3:
;; Read the expressions to compile from a resource file

(def ^:const expression-rsrc
  "./src/clj/algorithms_clj/macro_initiation_exprs.edn")

(defn rsrc->compiled-expressions
  [exprs]
  (for [{:keys [name definition]} exprs]
    `(def-expr ~name ~definition)))

(defmacro compile-resource-expressions
  []
  (let [compiled (-> (slurp expression-rsrc) read-string rsrc->compiled-expressions)]
    `(do ~@compiled)))

(compile-resource-expressions)

(deftest test-def-expr-rsrc
  (let [e (map->GravitationalPull {:m1 2 :m2 3 :distance 2})]
    (is (= 3/2 (eval-expr e)))
    (is (= '[/ [* :m1 :m2] [* :distance :distance]] (expr->data e)))
    ))

;; --------------------------------------------------------
;; Running the tests
;; --------------------------------------------------------

(test/run-tests)
