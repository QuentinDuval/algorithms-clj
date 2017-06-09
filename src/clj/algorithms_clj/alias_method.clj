(ns algorithms-clj.alias-method
  (:require
    [clojure.test :as test :refer [deftest is are testing]]
    [clojure.test.check :as tc]
    [clojure.test.check.generators :as tc-gen]
    [clojure.test.check.properties :as tc-prop]
    [clojure.test.check.clojure-test :refer [defspec]]
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

(defn ^:private build-alias-array
  "Preprocessing phase of the Alias Algorithm

   Build the array of the alias method in O(N log N) complexity:
   - Input: a sequence of pairs [value associated-weighted]
   - Output: an array of tuples [value-1 value-2 p1-over-p2]

   Resources:
   - https://stackoverflow.com/questions/6409652/random-weighted-selection-in-java
   - https://oroboro.com/non-uniform-random-numbers/"
  [weighted-pairs]
  (let [sum-weights (transduce (map second) + weighted-pairs)
        avg-weight (/ sum-weights (dec (count weighted-pairs)))]
    (loop [weighted-pairs (into (sorted-set) (map (comp vec reverse)) weighted-pairs)
           result []]
      (if (<= 2 (count weighted-pairs))
        (let [[w-least v-least :as least] (first weighted-pairs)
              [w-most v-most :as most] (last weighted-pairs)
              weighted-pairs (disj weighted-pairs least most)
              remaining-weight (- w-most (- avg-weight w-least))
              result (conj result [v-least v-most (/ w-least avg-weight)])]
          (if (zero? remaining-weight)
            (recur weighted-pairs result)
            (recur (conj weighted-pairs [remaining-weight v-most]) result)
            ))
        result))))

(defn enumerated-distribution-gen
  "Create a random generator producing weighted inputs
   - Input: a sequence of pairs [value associated-weighted]
   - Output: a random generator"
  [weighted-pairs]
  {:pre [(pos? (count weighted-pairs))]}
  (if (= 1 (count weighted-pairs))
    (constantly (-> weighted-pairs first first))
    (let [aliases (build-alias-array weighted-pairs)]
      (fn alias-gen []
        (let [[v1 v2 p] (rand-nth aliases)]
          (if (< (rand) p) v1 v2)))
      )))


; -----------------------------------------------------------------------------
; Unit tests
; -----------------------------------------------------------------------------

(deftest test-enumarated-dist->aliases
  (are [expected input] (= expected (build-alias-array input))
    [[:a :b 1/2]] {:a 1 :b 1}
    [[:a :b 1/3]] {:a 1 :b 2}
    [[:a :c 1/2] [:b :c 1/2]] {:a 1 :b 1 :c 2}
    [[:C :F 0.2]
     [:E :A 0.48]
     [:F :B 0.5999999999999999]
     [:B :A 0.3999999999999999]] {:A 0.28 :B 0.20 :C 0.05 :E 0.12 :F 0.35}
    [[:a :f 5/6]
     [:f :e 2/3]
     [:e :d 1/2]
     [:d :c 1/3]
     [:c :b 1/6]] {:a 1 :b 1 :c 1 :d 1 :e 1 :f 1}
    ))

(def enumerated-distribution-input-gen
  (tc-gen/such-that
    #(< 1 (count %))
    (tc-gen/map
      tc-gen/string
      (tc-gen/such-that pos? tc-gen/nat))))

(defspec alias-array-should-have-one-less-element
  100
  (tc-prop/for-all [enum-dist enumerated-distribution-input-gen]
    (=
      (dec (count enum-dist))
      (count (build-alias-array enum-dist)))
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

; TODO - Property based testing: the output should be one less in size

