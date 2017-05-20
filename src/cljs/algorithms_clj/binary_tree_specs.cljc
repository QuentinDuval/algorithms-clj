(ns cljs.algorithms-clj.binary-tree-specs
  (:require
    [cljs.spec.alpha :as s]
    ))


(def sample-tree
  [1
   {:left [2 {}]
    :right [3
            {:left [4 {}]
             :right [5 {}]}]
    }])

(s/def ::binary-tree-impl
  (s/cat
    :value any?
    :children
    (s/map-of #{:left :right} ::binary-tree-impl)))

(defn dfs-binary-tree
  [b-tree]
  (if (nil? b-tree)
    []
    (concat
      (dfs-binary-tree (-> b-tree second :left))
      [(first b-tree)]
      (dfs-binary-tree (-> b-tree second :right))
      )))

(defmacro binary-tree-of
  [pred]
  `(s/and
     (comp (partial every? ~pred) dfs-binary-tree)
     ::binary-tree-impl))

(s/def ::int-binary-tree
  (s/and
    (comp (partial every? int?) dfs-binary-tree)
    ::binary-tree-impl))