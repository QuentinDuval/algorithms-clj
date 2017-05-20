(ns algorithms-clj.binary-tree-macros)

(defmacro def-b-tree
  [name pred]
  `(s/def ~name
     (s/cat
       :value ~pred
       :children
       (s/map-of #{:left :right} ~name))))

(defmacro binary-tree-of
  [pred]
  `(s/and
     (comp (partial every? ~pred)
       algorithms-clj.binary-tree-specs/dfs-binary-tree)
     :algorithms-clj.binary-tree-specs/binary-tree-impl))