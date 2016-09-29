(ns algorithms-clj.ancestors
  (:require
    [clojure.core.logic :rename {== ===} :refer :all]
    [clojure.core.logic.fd :as fd]
    ))


(run* [q]
  (fresh [a b]
    (membero a [1 3 6])
    (fd/in b (fd/interval 0 10))
    (fd/+ a b 6)
    (fd/<= q b)
    (=== a q)
    ))


