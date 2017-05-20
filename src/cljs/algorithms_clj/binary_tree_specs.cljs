(ns algorithms-clj.binary-tree-specs
  (:require-macros
    [algorithms-clj.binary-tree-macros :refer [binary-tree-of]])
  (:require
    [cljs.spec.alpha :as s]))

(def sample-tree
  [1
   {:left [2 {}]
    :right [3
            {:left [4 {}]
             :right [5 {}]}]
    }])

(defn dfs-binary-tree
  [b-tree]
  (if (nil? b-tree)
    []
    (concat
      (dfs-binary-tree (-> b-tree second :left))
      [(first b-tree)]
      (dfs-binary-tree (-> b-tree second :right))
      )))

(s/def ::binary-tree-impl
  (s/cat
    :value any?
    :children
    (s/map-of #{:left :right} ::binary-tree-impl)))

(s/def ::int-binary-tree
  (s/and
    (comp (partial every? int?) dfs-binary-tree)
    ::binary-tree-impl))

(s/def ::int-binary-tree-macro
  (binary-tree-of int?))

(println (s/valid? ::int-binary-tree sample-tree))
(println (s/valid? ::int-binary-tree-macro sample-tree))
