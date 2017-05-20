(ns cljs.algorithms-clj.binary-tree-specs
  (:require
    [cljs.spec.alpha :as s]
    ))

(def sample-tree-2
  [1
   {:left [2 {}]
    :right [3
            {:left [4 {}]
             :right [5 {}]}]
    }])

(s/def ::int-binary-tree-macro
  (binary-tree-of int?))