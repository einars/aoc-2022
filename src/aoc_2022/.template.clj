(ns aoc-2022.dayN
  (:require [clojure.test :as test :refer [deftest]])
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:require [clojure.tools.trace :refer [trace deftrace]])
  (:require [clojure.pprint :as pp])
  (:require [aoc.helpers :as h]))

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
    ; 0 (solve-2 "resources/2022/dayN.test.txt")
    ))
