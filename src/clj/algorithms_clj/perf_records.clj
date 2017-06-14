(ns algorithms-clj.perf-records
  (:require [criterium.core :as perf]))

(set! *warn-on-reflection* true)

(defrecord Planet
  [position mass speed])

(defrecord Planet-2
  [^double position ^double mass ^double speed])

(defn cinetic-energy-1
  [planet]
  (* 1/2 (:mass planet) (:speed planet) (:speed planet)))

(defn cinetic-energy-rec
  [^Planet planet]
  (* 1/2 (.-mass planet) (.-speed planet) (.-speed planet)))

(defn cinetic-energy-rec-2
  [^Planet-2 planet]
  (* 1/2 (.-mass planet) (.-speed planet) (.-speed planet)))

; -----------------------------------------------

(defn run-bench*
  [name f n]
  (println "\n" name ": ------------------------------")
  (perf/quick-bench (f n)))

(defmacro run-bench
  [[f n]]
  `(run-bench* ~(name f) ~f ~n))

(defn run-benches
  []
  (let [junk-data (into {} (for [i (range 1000)] [i i]))
        planet-as-map {:position 0.0 :mass 123.1 :speed 75.2}
        planet-as-rec1 (map->Planet planet-as-map)
        planet-as-rec2 (map->Planet-2 planet-as-map)

        planet-as-map-b (reduce conj planet-as-map junk-data)
        planet-as-rec1b (reduce conj planet-as-rec1 junk-data)
        planet-as-rec2b (reduce conj planet-as-rec2 junk-data)]

    (println "********** WITHOUT JUNK DATA ***********")
    (run-bench (cinetic-energy-1 planet-as-map))
    (run-bench (cinetic-energy-1 planet-as-rec1))
    (run-bench (cinetic-energy-1 planet-as-rec2))
    (run-bench (cinetic-energy-rec planet-as-rec1))
    (run-bench (cinetic-energy-rec-2 planet-as-rec2))

    (println "********** WITH JUNK DATA ***********")
    (run-bench (cinetic-energy-1 planet-as-map-b))
    (run-bench (cinetic-energy-1 planet-as-rec1b))
    (run-bench (cinetic-energy-1 planet-as-rec2b))
    (run-bench (cinetic-energy-rec planet-as-rec1b))
    (run-bench (cinetic-energy-rec-2 planet-as-rec2b))
    ))
