(ns aoc-2022.day15
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn parse [l]
  (let [[sx sy bx by] (->> l
                        (re-seq #"-?\d+")
                        (mapv #(Integer/parseInt %)))]
    [sx sy (+ (abs (- bx sx)) (abs (- by sy))) bx by] ))

(defn get-busy-range [beacon target-y]
  (let [[sx sy dist] beacon
        dist-at-y (abs (- target-y sy))]

    (when (<= dist-at-y dist)
      (let [side (- dist dist-at-y)]
        [(- sx side) (+ sx side)]))))

(defn solve-for
  [y beacons]
  (let [intervals (keep #(get-busy-range % y) beacons)
        beacon-xs-at-y (->> beacons
                         (filter #(= y (nth % 4)))
                         (map #(nth % 3)))]
    (set/difference 
      (into #{} (mapcat #(range (first %) (inc (second %))) intervals))
      (into #{} beacon-xs-at-y))))

(defn in-safe-range-one? [[x y] [sx sy dist bx by]]
  (let [our-distance (+ (abs (- x sx)) (abs (- y sy)))]
    (or (<= our-distance dist) (and (= x bx) (= y by)))))

(defn in-safe-range? [xy beacons]
  (some #(in-safe-range-one? xy %) beacons))

(defn get-outer-border [[sx sy dist]] 
  "all the coordinates immediately outside the safe range"
  (let [dist (inc dist)]
    (apply concat (for [n (range 0 (inc dist))]
                    [[(+ sx (- dist n)) (- sy n)]
                     [(+ sx (- dist n)) (+ sy n)]
                     [(- sx (- dist n)) (- sy n)]
                     [(- sx (- dist n)) (+ sy n)]]))))

(defn find-distress [in-range? beacons]
  (->> beacons
    (mapcat get-outer-border)
    (filter (fn [[x y]] (and (in-range? x) (in-range? y)))) ; clip to requested coordinates
    (filter #(not (in-safe-range? % beacons))) ; remove coordinates deemed safe
    first))

(defn calc-tuning [[x y]] (+ y (* 4000000 x)))

(defn solve-1
  ([] (solve-1 "resources/2022/day15.txt" 2000000))
  ([file y]
   (->> file
     (h/slurp-strings)
     (map parse)
     (solve-for y)
     count)))

(defn solve-2
  ([] (solve-2 "resources/2022/day15.txt" #(< 0 % 4000000)))
  ([file in-range?]
   (->>
     (h/slurp-strings file)
     (map parse)
     (find-distress in-range?)
     (calc-tuning))))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    26 (solve-1 "resources/2022/day15.test.txt" 10)
    25 (solve-1 "resources/2022/day15.test.txt" 9)
    56000011 (solve-2 "resources/2022/day15.test.txt" #(< 0 % 20))))
