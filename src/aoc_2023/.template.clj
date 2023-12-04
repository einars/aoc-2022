(ns aoc-2023.dayNNN
  (:require
    [clojure.test :as test :refer [deftest]]
    [aoc.helpers :as h]))

(def sample-data [])

(defn solve-1
  ([] (solve-1 (slurp "resources/2023/dayNNN.txt"))
  ([m] false)))

(defn solve-2
  ([] (solve-2 (slurp "resources/2023/dayNNN.txt")))
  ([m] false))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    0 (solve-1 sample-data)
    0 (solve-2 sample-data)
    ))

(comment
  (solve-1)
  (solve-2))
