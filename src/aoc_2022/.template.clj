(ns aoc-2022.dayN
  (:require [clojure.test :as test :refer [deftest]])
  (:require [aoc-2022.helpers :as h]))

(defn solve-1
  ([] (solve-1 "resources/dayN.txt"))
  ([file]
    (->>
      (h/slurp-strings file)
      ; ...
      )))

(comment defn solve-2
  ([] (solve-2 "resources/dayN.txt"))
  ([file]
    (->>
      (h/slurp-strings file)
      ; ...
      )))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    true false
    ; 0 (solve-1 "resources/dayN.test.txt")
    ; 0 (solve-2 "resources/dayN.test.txt")
    ))
