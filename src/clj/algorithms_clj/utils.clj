(ns algorithms-clj.utils)


(defn map-first
  ([f] (map (fn [[a b]] [(f a) b])))
  ([f c] (eduction (map-first f) c)))

(defn iterate-until-single-value
  "Apply a reducing step on a collection until its size is equal to 1, and return that element"
  [step collection]
  (loop [result collection]
    (if (< 1 (count result))
      (recur (step result))
      (first result))
    ))

(defmacro reduce-to-single-value
  [[collection] & body]
  `(iterate-until-single-value
     (fn [~collection] ~@body) ~collection))

(defn rotations
  "Generates all the rotations of an input sequence"
  [inputs]
  (let [n (count inputs)
        xf (comp (map #(take n %)) (take n))]
    (eduction
      xf
      (iterate #(drop 1 %) (concat inputs inputs)))
    ))

(defmacro make-transducer
  "Build a transducer, based on
  - a list of bindings (inner state)
  - all the bodies of the step function"
  [[xf] bindings & step-fn]
  `(fn [~xf]
     (let ~bindings
       (fn ~@step-fn))))

(defmacro make-simple-transducer
  "Build a simple step function for a transducer, based on
  - a list of bindings (inner state)
  - the body of a step function taking two arguments"
  [[xf] bindings & step-fn]
  `(make-transducer
     [~xf] ~bindings
     ([] (~xf))
     ([result#] (~xf result#))
     ~@step-fn))

