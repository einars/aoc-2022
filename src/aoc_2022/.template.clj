(ns aoc-2022.dayN
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn solve-1
  ([] (solve-1 "resources/2022/dayN.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     ; ...
     )))


(deftest test-stuff [] 
  (test/are [x y] (= x y)
    true false
    ; 0 (solve-1 "resources/2022/dayN.test.txt")
    ))
