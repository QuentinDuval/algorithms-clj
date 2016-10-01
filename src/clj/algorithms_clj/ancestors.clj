(ns algorithms-clj.ancestors
  (:require
    [clojure.core.logic :rename {== ===} :refer :all]
    [clojure.core.logic.fd :as fd]
    [clojure.core.logic.pldb :as pldb]
    ))


;; ---------------------------------------------------------
;; DEBUG - TRY TO HAVE "EXISTS"
;; ---------------------------------------------------------

(defn test-dbg
  []
  (pldb/db-rel link src dst)
  
  (def dbg-facts
    (pldb/db
      [link 'a 'b]
      [link 'a 'c]
      [link 'b 'c]
      [link 'b 'd]
      ))
  
  (defn has-parent?
    [x]
    (all
      (fresh [z] (link z x))
      ))
  
  (pldb/with-db dbg-facts
    (run* [childs]
      (has-parent? childs))
    ))


;; ---------------------------------------------------------
;; Classic ancestor prolog demonstration program
;; ---------------------------------------------------------

(pldb/db-rel male h)
(pldb/db-rel female h)
(pldb/db-rel parent p c)

(def family-facts-db
  (pldb/db
    [male 'John]
    [male 'Herb]
    [male 'Mike]
    [male 'Philip] 
    [female 'Alice]
    [female 'Jessy]
    [female 'Lisa]
    [female 'Nicola]
    
    [parent 'John 'Alice]
    [parent 'John 'Mike]
    [parent 'John 'Nicola]
    [parent 'John 'Philip]
    
    [parent 'Jessy 'Alice]
    [parent 'Jessy 'Mike]
    [parent 'Jessy 'Nicola]
    
    [parent 'Mike 'Herb]
    [parent 'Alice 'Herb]
    [parent 'Alice 'Lisa]
    ))

(defn father [x y] (all (parent x y) (male x)))
(defn mother [x y] (all (parent x y) (female x)))

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
    ;; TODO - CUT: one child is enough but it keeps exploring and reporting the same couple
    ;; defna does not work - defnu cut at the first finding, for all siblings!
    ;; The notion to express is "there exists" instead of "for all": HOW?
    (father x z) (mother y z)
    ))

(defn get-couples []
  (run* [parents]
    (fresh [x y]
      (have-child? x y)
      (=== parents [x y]))
    ))

(defn full-siblings?
  "Identify whether two individuals have the same parents"
  [x y]
  (fresh [p1 p2]
    (!= x y)
    (father p1 x) (mother p2 x)
    (father p1 y) (mother p2 y)
    ))

(defn check-full-siblings?
  [x y]
  (not (empty?
         (run 1 [r] (full-siblings? x y))
         )))

(defn get-all-siblings []
  (run* [c1 c2]
    (full-siblings? c1 c2)
    ))

(defn tests []
  (pldb/with-db family-facts-db
    (println (get-ancestors 'Herb))
    (println (get-grand-parents 'Herb))
    (println (get-couples))
    (println (get-all-siblings))
    (println (check-full-siblings? 'Alice 'Mike))
    (println (check-full-siblings? 'Alice 'Philip))
    ))

