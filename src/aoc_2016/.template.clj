(ns aoc-2016.dayN
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pprint]
    [aoc.helpers :as h]))

(defn solve-1
  ([] (solve-1 "resources/2016/dayN.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     ; ...
     )))

(comment defn solve-2
  ([] (solve-2 "resources/2016/dayN.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     ; ...
     )))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    true false
    ; 0 (solve-1 "resources/2016/dayN.test.txt")
    ; 0 (solve-2 "resources/2016/dayN.test.txt")
    ))
