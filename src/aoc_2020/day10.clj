(ns aoc-2020.day10
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn get-answer [diffs]
  (let [diffs (conj diffs 3)
        grouped (group-by identity diffs)]
    (* (count (get grouped 3 0)) (count (get grouped 1 0)))))

(defn add-0 [xs] (conj xs 0))

(defn solve-1
  ([] (solve-1 "resources/2020/day10.txt"))
  ([file]
   (->>
     (h/slurp-ints file)
     add-0
     (sort >)
     (partition 2 1)
     (map (partial apply -))
     get-answer
     )))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    35 (solve-1 "resources/2020/day10.test.txt")
    220 (solve-1 "resources/2020/day10.test2.txt")))
