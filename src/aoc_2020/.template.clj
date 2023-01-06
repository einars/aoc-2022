(ns aoc-2020.dayN
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as string]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn solve-1
  ([] (solve-1 "resources/2020/dayN.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     ; ...
     )))


(deftest test-stuff [] 
  (test/are [x y] (= x y)
    0 (solve-1 "resources/2020/dayN.test.txt")))
