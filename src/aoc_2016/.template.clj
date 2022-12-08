(ns aoc-2016.dayN
  (:require [clojure.test :as test :refer [deftest]])
  (:require [clojure.string :as str])
  (:require [clojure.tools.trace :refer [trace deftrace]])
  (:require [clojure.pprint :as pprint])
  (:require [aoc.helpers :as h]))

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
