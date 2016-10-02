(ns algorithms-clj.ancestors
  (:require
    [clojure.core.logic :rename {== ===} :refer :all]
    [clojure.core.logic.fd :as fd]
    [clojure.core.logic.pldb :as pldb]
    ))


;; ---------------------------------------------------------
;; DEBUG - TRY TO HAVE "EXISTS"
;; ---------------------------------------------------------

;; Equivalent to:
;;
;; parent(a,b).
;; parent(a,c).
;; parent(b,c).
;; parent(b,d).
;; is_child(C):-parent(_,C).
;; 
;; is_child(X) => returns the duplicate for c

;; If you modify with a cut:
;; is_child(C):-parent(_,C),!.
;; => is_child(X) => returns only one result => it becomes the query "there exists a child"

;; http://kti.ms.mff.cuni.cz/~bartak/prolog/how.html

(defn test-dbg-1
  "First modelization: parent several times"
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
    (fresh [z] (link z x)))
  
  (pldb/with-db dbg-facts
    (run* [childs]
      (has-parent? childs))
    ))


(defn test-dbg-2
  "Different modelization: parent of a bunch of childs" 
  []
  (pldb/db-rel link src dst)
  
  (def dbg-facts
    (pldb/db
      [link 'a ['b 'c]] ;; Sets would be nicer, but seems not supported in core.logic
      [link 'b ['c 'd]]
      ))
  
  (defn has-parent?
    [x]
    (fresh [parent childs]
      (link parent childs)
      (member1o x childs)
      ))
  
  (pldb/with-db dbg-facts
    (run* [childs]
      (has-parent? childs))
    ))


(defn test-dbg-3
  "Emulation of logic program requires to create 'views' to store each side of a relation"
  []
  (def db
    {:people #{:a :b :c :d}
     :parent-rels #{[:a :b] [:a :c] [:b :c] [:b :d]}
     })
  
  (def parent-view (into {} (:parent-rels db)))
  (def childs-view (into {} (map (comp vec reverse)) (:parent-rels db)))
  
  (defn has-parent? [c]
    (contains? childs-view c))
  
  (keys childs-view))



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

