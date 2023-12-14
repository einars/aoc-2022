(ns aoc-2023.dayN
  (:require
    [clojure.test :as test :refer [deftest are]]
    [clojure.string :as str]
    [aoc.helpers :as h]))

(def sample-data [])
(def input-file "resources/2023/dayN.txt")

(defn solve-1
  ([] (solve-1 (h/slurp-strings input-file)))
  ([m] (->> m
            )))

(defn solve-2
  ([] (solve-2 (h/slurp-strings input-file)))
  ([m] (->> m
            )))

(deftest test-stuff [] 
  (are [x y] (= x y)
    0 (solve-1 sample-data)
    0 (solve-2 sample-data)))

(comment
  (solve-1)
  (solve-2))
