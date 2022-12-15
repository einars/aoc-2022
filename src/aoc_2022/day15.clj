(ns aoc-2022.day15
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn parse [l]
  (->> l
    (re-seq #"-?\d+")
    (mapv #(Integer/parseInt %))))

(defn get-busy-range [beacon target-y]
  (let [[sx sy bx by] beacon
        safe-distance (+ (abs (- bx sx)) (abs (- by sy)))
        dist-at-y (abs (- target-y sy))]

    (when (<= dist-at-y safe-distance)
      (let [side (- safe-distance dist-at-y)]
        [(- sx side) (+ sx side) :bea beacon :safe safe-distance :dist dist-at-y]))))

(defn solve-for
  [y beacons]
  (let [intervals (keep #(get-busy-range % y) beacons)
        beacon-xs-at-y (->> beacons
                         (filter #(= y (nth % 3)))
                         (map #(nth % 2)))
        sensor-xs-at-y (->> beacons
                         (filter #(= y (nth % 1)))
                         (map #(nth % 0)))
        ]
    (count (set/difference 
      (into #{} (sort (mapcat #(range (first %) (inc (second %))) intervals)))
      (into #{} beacon-xs-at-y)
      (into #{} sensor-xs-at-y)))))

(defn solve-1
  ([] (solve-1 "resources/2022/day15.txt" 2000000))
  ([file y]
   (->>
     (h/slurp-strings file)
     (map parse)
     (solve-for y))))


(deftest test-stuff [] 
  (test/are [x y] (= x y)
    27 (solve-1 "resources/2022/day15.test.txt" 11)
    26 (solve-1 "resources/2022/day15.test.txt" 10)
    25 (solve-1 "resources/2022/day15.test.txt" 9)
    ))
