(ns algorithms-clj.ancestors
  (:require
    [clojure.core.logic :rename {== ===} :refer :all]
    [clojure.core.logic.fd :as fd]
    [clojure.core.logic.pldb :as pldb]
    ))


(defn first-test
  "Show how you can bind values to the logic expression from the outside"
  [values target-sum]
  (run* [a]
   (fresh [b]
     (membero a values)
     (membero b values)
     (fd/<= b a)
     (fd/+ a b target-sum)
     )))


;; ---------------------------------------------------------
;; Classic ancestor prolog
;; ---------------------------------------------------------

(pldb/db-rel male h)
(pldb/db-rel female h)
(pldb/db-rel parent p c)

(def family-facts-db
  (pldb/db
    [male 'John]
    [male 'Herb]
    [female 'Alice]
    [female 'Jessy]
    [parent 'John 'Alice]
    [parent 'Jessy 'Alice]
    [parent 'Alice 'Herb]
    ))

(defn is-grand-parent?
  "Indicates whether x is a grand-parent of y"
  [x y]
  (fresh [z]
    (parent x z)
    (parent z y)
    ))

(defn get-grand-parents [person]
  (run* [ancestor] (is-grand-parent? ancestor person)))

(defn is-ancestor?
  "Indicates whether x is an ancestor of y"
  [x y]
  (conde
    [(parent x y)]
    [(fresh [z] (parent x z) (is-ancestor? z y))]
    ))

(defn get-ancestors [person]
  (run* [ancestor] (is-ancestor? ancestor person)))

(defn have-child?
  "Indicates whether x and y had a child together"
  [x y]
  (fresh [z]
    (project [x y]
      (compare x y))
    ;;(!= x y)
    (parent x z)
    (parent y z)
    ))

(defn get-couples []
  (run* [parents]
    (fresh [x y]
      (have-child? x y)
      (=== parents [x y]))
    ))

(defn tests []
  (pldb/with-db family-facts-db
    (println (get-ancestors 'Herb))
    (println (get-grand-parents 'Herb))
    (println (get-couples))
    ))

