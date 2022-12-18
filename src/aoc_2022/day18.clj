(ns aoc-2022.day18
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn parse-xyz [s]
  (map #(Integer/parseInt %) (str/split s #",")))

(defn count-open-sides 
  [lava [x y z]]
  (+ 
    (if (lava [(inc x) y z]) 0 1)
    (if (lava [(dec x) y z]) 0 1)
    (if (lava [x (inc y) z]) 0 1)
    (if (lava [x (dec y) z]) 0 1)
    (if (lava [x y (inc z)]) 0 1)
    (if (lava [x y (inc z)]) 0 1)))

(defn count-all-open-sides [lava]
  (reduce + (map #(count-open-sides lava %) lava)))

(defn solve-1
  ([] (solve-1 "resources/2022/day18.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (map parse-xyz)
     (set)
     (count-all-open-sides))))


(deftest test-stuff [] 
  (test/are [x y] (= x y)
    true false
    10 (count-all-open-sides #{[1 1 1] [2 1 1]})
    64 (solve-1 "resources/2022/day18.test.txt")))
