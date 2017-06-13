(ns algorithms-clj.alias-method
  (:require
    [clojure.test :as test :refer [deftest is are testing]]
    [clojure.test.check :as tc]
    [clojure.test.check.generators :as tc-gen]
    [clojure.test.check.properties :as tc-prop]
    [clojure.test.check.clojure-test :refer [defspec]]
    [algorithms-clj.utils :refer [map-values]]
    ))



(defn ^:private sum-weights
  [enum-dist]
  (transduce (map second) + enum-dist))

(defn linear-enumerated-distribution-gen
  [enum-dist]
  (let [total-weight (sum-weights enum-dist)]
    (fn []
      (loop [searched-weight (rand total-weight)
             [[value weight] & enum-dist] (seq enum-dist)]
        (if (<= weight searched-weight)
          (recur (- searched-weight weight) enum-dist)
          value)))))

(defn ^:private normalize-enum-dist
  [enum-dist]
  (let [total-weight (sum-weights enum-dist)]
    (map-values (fn [v] (/ v total-weight)) enum-dist)))

(defn ^:private buckets->enum-dist
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

(defn ^:private fill-one-bucket
  "Takes the quantity bucket-vol from a sorted-set of elements to verse
   Returns a filled bucket and the remaining quantities"
  [to-verse bucket-vol]
  (let [[min-vol min-value :as min-dist] (first to-verse)
        [max-vol max-value :as max-dist] (last to-verse)
        fill-bucket [min-value max-value (/ min-vol bucket-vol)]
        rest-vol (- max-vol (- bucket-vol min-vol))
        to-verse (disj to-verse min-dist max-dist)
        to-verse (if (pos? rest-vol)
                   (conj to-verse [rest-vol max-value])
                   to-verse)]
    [fill-bucket to-verse]))

(defn ^:private enum-dist->buckets
  "Preprocessing phase of the Alias Algorithm

   Build a vector of bucket in O(N log N) complexity:
   - Input: a sequence of pairs [value associated-weighted]
   - Output: a vector of tuples [value-1 value-2 p1-over-p2]

   Resources:
   - https://stackoverflow.com/questions/6409652/random-weighted-selection-in-java
   - https://oroboro.com/non-uniform-random-numbers/"
  [enum-dist]
  (let [bucket-nb (dec (count enum-dist))   ; Number of buckets to fill
        total-vol (sum-weights enum-dist)   ; Total volume split over the buckets
        bucket-vol (/ total-vol bucket-nb)] ; Volumne of each bucket
    (loop [to-verse (into (sorted-set)      ; Remaining quantity to verse in the buckets
                     (map (comp vec reverse))
                     enum-dist)
           buckets []]                      ; The filled buckets to return
      (if (<= 2 (count to-verse))
        (let [[bucket to-verse] (fill-one-bucket to-verse bucket-vol)]
          (recur to-verse (conj buckets bucket)))
        buckets))))

(defn enumerated-distribution-gen
  "Create a random generator producing weighted inputs
   - Input: a sequence of pairs [value weight-of-value]
   - Output: a random generator that picks values from the input
     such that P(value) = Weight(value) / Sum(all weights)"
  [enum-dist]
  {:pre [(pos? (count enum-dist))]}
  (if (= 1 (count enum-dist))
    (constantly (ffirst enum-dist))
    (let [buckets (enum-dist->buckets enum-dist)]
      (fn []
        (let [[v1 v2 p] (rand-nth buckets)]
          (if (< (rand) p) v1 v2)))
      )))


; -----------------------------------------------------------------------------
; Unit tests
; -----------------------------------------------------------------------------

(deftest test-enum-dist->buckets
  (are [expected input] (= expected (enum-dist->buckets input))
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

(defspec bucket-array-should-have-one-less-element
  100
  (tc-prop/for-all [enum-dist enumerated-distribution-test-gen]
    (=
      (dec (count enum-dist))
      (count (enum-dist->buckets enum-dist)))
    ))

(defspec bucket-array-should-perserve-probability-volume
  (tc-prop/for-all [enum-dist enumerated-distribution-test-gen]
    (=
      (normalize-enum-dist enum-dist)
      (buckets->enum-dist (enum-dist->buckets enum-dist)))
    ))


; -----------------------------------------------------------------------------
; High level tests
; -----------------------------------------------------------------------------

(defn run-prob-test
  []
  (let [enum-dist {:a 1 :b 1 :c 4 :d 1 :e 1}
        test-gen (fn [gen] (frequencies (repeatedly 10000 gen)))]
    (prn (test-gen (enumerated-distribution-gen enum-dist)))
    (prn (test-gen (linear-enumerated-distribution-gen enum-dist)))
    ))
