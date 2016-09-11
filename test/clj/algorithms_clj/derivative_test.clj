(ns algorithms-clj.derivative-test
  (:require
    [algorithms-clj.derivative :refer :all]
    [clojure.test :refer :all]
    [clojure.test.check :as tc]
    [clojure.test.check.clojure-test :refer [defspec]]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [midje.sweet :refer :all]
    ))

(deftest test-simple-terms
  (fact "Constants yields 0"
    (derivative 1 'x) => 0
    (derivative 'x 'y) => 0
    (derivative :x :y) => 0
    (derivative :x 'y) => 0
    )
  (fact "Variable yields 1"
    (derivative 'x 'x) => 1
    (derivative :x :x) => 1
    ))

(deftest test-sum-of-terms
  (fact "Summing primitive terms"
    (derivative [+ 1 2] :x) => 0
    (derivative [+ :x 2] :y) => 0
    (derivative [+ :x] :y) => 0
    )
  (fact "Summing variable and primitives"
    (derivative [+ :x :y] :x) => 1
    (derivative [+ :x :x] :x) => 2
    (derivative [+ :x] :x) => 1
    ))

(deftest test-product-of-terms
  (fact "Product of primitive terms"
    (derivative [* 1 20] :x) => 0
    (derivative [* :y 2] :x) => 0
    )
  (fact "Product of variable and primitives"
    (derivative [* :x 1] :x) => 1
    (derivative [* 2 :x] :x) => 2
    )
  (fact "Product of multiple variables"
    (derivative [* :x :y] :x) => :y
    ;;(derivative [* :x :y :z] :x) => [* :y :z] ;; FIX
    (derivative [* :x :x :x] :x) => [+ :x :x :x] ;; TODO: Perfect
    )
  )

(deftest test-product-sum-composites
  (fact "Sum of product"
    ;;(derivative [+ [* :x :y] [:x :x]] :x) => [+ :y [+ :x :x]] ;; FIX
    )
  )

(run-tests)
