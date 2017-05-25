(ns algorithms-clj.binary-tree-specs)

(defmacro def-b-tree
  [name pred]
  `(s/def ~name
     (s/cat
       :value ~pred
       :children
       (s/map-of #{:left :right} ~name))))

#_(defmacro binary-tree-of
  [pred]
  `(s/and
     #(every? ~pred (dfs-binary-tree %))
     ::binary-tree-impl))

(defmacro binary-tree-of
  [pred]
  `(s/and
     #(s/valid? ::binary-tree-impl %)
     #(every? (partial s/valid? ~pred) (dfs-binary-tree %))
     ))