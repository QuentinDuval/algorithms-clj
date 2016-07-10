(ns algorithms-clj.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [algorithms-clj.core-test]
   [algorithms-clj.common-test]))

(enable-console-print!)

(doo-tests 'algorithms-clj.core-test
           'algorithms-clj.common-test)
