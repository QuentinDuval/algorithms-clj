(ns algorithms-clj.binary-tree-macros)

(defmacro binary-tree-of
  [pred]
  `(s/and
     (comp (partial every? ~pred)
       algorithms-clj.binary-tree-specs/dfs-binary-tree)
     :algorithms-clj.binary-tree-specs/binary-tree-impl))