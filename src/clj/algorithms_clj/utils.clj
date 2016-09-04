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