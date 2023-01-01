(ns aoc-2020.day13
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn parse-schedule [sched]
  (->> (str/split sched #",")
    (filter (partial not= "x" ))
    (map #(Integer/parseInt %))))

(defn parse [[n sched]]
  [(Integer/parseInt n) (parse-schedule sched)])

(defn get-wait-times [[n sched]]
  (map (fn [bus] [bus (- bus (mod n bus))]) sched))

(defn solve-1
  ([] (solve-1 "resources/2020/day13.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     parse
     get-wait-times
     (sort-by second)
     first
     (apply *))))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    295 (solve-1 "resources/2020/day13.test.txt")))
