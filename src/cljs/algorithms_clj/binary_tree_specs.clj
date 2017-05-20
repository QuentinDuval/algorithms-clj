(ns algorithms-clj.binary-tree-specs)

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
     #(every? ~pred (dfs-binary-tree %))
     ::binary-tree-impl))