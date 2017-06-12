(ns algorithms-clj.alias-method
  (:require
    [clojure.test :as test :refer [deftest is are testing]]
    [clojure.test.check :as tc]
    [clojure.test.check.generators :as tc-gen]
    [clojure.test.check.properties :as tc-prop]
    [clojure.test.check.clojure-test :refer [defspec]]
    [algorithms-clj.utils :refer [map-values]]
    ))


#_(defn linear-enumerated-distribution-gen
    [m]
    (let [weights (reductions + (vals m))
          total (last weights)
          choices (map vector (keys m) weights)]
      (fn []
        (let [choice (rand-int total)]
          (loop [[[val w] & more] choices]
            (if (< choice w) val (recur more)))))))

(defn ^:private sum-weights
  [enum-dist]
  (transduce (map second) + enum-dist))

(defn ^:private normalize-enum-dist
  [enum-dist]
  (let [total-weight (sum-weights enum-dist)]
    (map-values (fn [v] (/ v total-weight)) enum-dist)))

(defn ^:private aliases->enum-dist
  [aliases]
  (let [den (count aliases)]
    (reduce
      (fn [probs [lhs rhs prob-lhs]]
        (merge-with +
          {lhs (/ prob-lhs den)
           rhs (/ (- 1 prob-lhs) den)}
          probs))
      {}
      aliases)))

(defn ^:private enum-dist->aliases
  "Preprocessing phase of the Alias Algorithm

   Build the array of the alias method in O(N log N) complexity:
   - Input: a sequence of pairs [value associated-weighted]
   - Output: an array of tuples [value-1 value-2 p1-over-p2]

   Resources:
   - https://stackoverflow.com/questions/6409652/random-weighted-selection-in-java
   - https://oroboro.com/non-uniform-random-numbers/"
  [enum-dist]
  (let [avg-weight (/ (sum-weights enum-dist) (dec (count enum-dist)))]
    (loop [enum-dist (into (sorted-set) (map (comp vec reverse)) enum-dist)
           result []]
      (if (<= 2 (count enum-dist))
        (let [[min-weight min-value :as min-dist] (first enum-dist)
              [max-weight max-value :as max-dist] (last enum-dist)
              remaining-weight (- max-weight (- avg-weight min-weight))
              enum-dist (disj enum-dist min-dist max-dist)
              enum-dist (if (pos? remaining-weight)
                          (conj enum-dist [remaining-weight max-value])
                          enum-dist)]
          (recur enum-dist
            (conj result [min-value max-value (/ min-weight avg-weight)])
            ))
        result))))

(defn enumerated-distribution-gen
  "Create a random generator producing weighted inputs
   - Input: a sequence of pairs [value associated-weighted]
   - Output: a random generator"
  [enum-dist]
  {:pre [(pos? (count enum-dist))]}
  (if (= 1 (count enum-dist))
    (constantly (-> enum-dist first first))
    (let [aliases (enum-dist->aliases enum-dist)]
      (fn alias-gen []
        (let [[v1 v2 p] (rand-nth aliases)]
          (if (< (rand) p) v1 v2)))
      )))


; -----------------------------------------------------------------------------
; Unit tests
; -----------------------------------------------------------------------------

(deftest test-enumarated-dist->aliases
  (are [expected input] (= expected (enum-dist->aliases input))
    [[:a :b 1/2]] {:a 1 :b 1}
    [[:a :b 1/3]] {:a 1 :b 2}
    [[:a :c 1/2] [:b :c 1/2]] {:a 1 :b 1 :c 2}
    [[:C :F 1/5]
     [:E :A 12/25]
     [:A :B 3/5]
     [:B :F 2/5]] {:A 28/100 :B 20/100 :C 5/100 :E 12/100 :F 35/100}
    [[:a :f 5/6]
     [:f :e 2/3]
     [:e :d 1/2]
     [:d :c 1/3]
     [:c :b 1/6]] {:a 1 :b 1 :c 1 :d 1 :e 1 :f 1}
    ))

(def enumerated-distribution-test-gen
  (tc-gen/such-that
    #(< 1 (count %))
    (tc-gen/map
      tc-gen/string
      (tc-gen/such-that pos? tc-gen/nat))))

(defspec alias-array-should-have-one-less-element
  100
  (tc-prop/for-all [enum-dist enumerated-distribution-test-gen]
    (=
      (dec (count enum-dist))
      (count (enum-dist->aliases enum-dist)))
    ))

(defspec alias-array-should-perserve-initial-probabilities
  (tc-prop/for-all [enum-dist enumerated-distribution-test-gen]
    (=
      (normalize-enum-dist enum-dist)
      (aliases->enum-dist (enum-dist->aliases enum-dist)))
    ))


; -----------------------------------------------------------------------------
; High level tests
; -----------------------------------------------------------------------------

(defn run-prob-test
  []
  (let [gen (enumerated-distribution-gen
              {:a 1 :b 1 :c 4 :d 1 :e 1})
        rolls (repeatedly 10000 gen)
        freqs (frequencies rolls)]
    (prn freqs)))
