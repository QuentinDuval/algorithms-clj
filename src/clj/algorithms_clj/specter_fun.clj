(ns algorithms-clj.specter-fun
  (:require
    [com.rpl.specter :refer :all]
    ))

(defn run-tests
  []
  (prn (transform [ALL :a even?] inc [{:a 1} {:a 3} {:a 5} {:a 3}]))
  (prn (setval [(srange 4 11)] [] (vec (range 15))))
  (prn (transform [(srange 4 11) (filterer even?)] reverse (vec (range 15))))
  (prn (transform [(filterer even?)] reverse (vec (range 15))))
  (prn (setval [(filterer even?)] [] (vec (range 15))))
  )
