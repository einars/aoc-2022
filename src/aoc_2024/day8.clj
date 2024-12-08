(ns aoc-2024.day8
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.tools.trace :refer :all]
   [clojure.math.combinatorics :as combo]
   [aoc.helpers :as h]))

(def input-txt "resources/2024/day8.txt")

(def sample
  (str/trim "
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"))

(defn parse-input
  [s]
  (let [m (-> s (str/split #"\n") h/make-xy-map)
        area (first m)
        antenna-types (set (vals area))
        antennas (into {} (mapv (fn [ant] 
                                  [ant (h/find-keys #(= ant %) area)]) antenna-types))
        ]

    {:dimensions (second m)
     :antenna-types antenna-types
     :antennas antennas
     }

    ))

(defn antinodes-for
  [[a1 a2]]
  (let [{x1 :x, y1 :y} a1
        {x2 :x, y2 :y} a2
        dx (- x2 x1)
        dy (- y2 y1)]

    [{:x (- x1 dx)
      :y (- y1 dy)}
     {:x (+ x2 dx)
      :y (+ y2 dy)}]))

(defn antinodes-for-all [antennas]
  (set (flatten (map antinodes-for (combo/combinations antennas 2)))))

(defn inside-dimensions?
  [dims pt]
  (let [{mx :x my :y} dims
        {px :x py :y} pt]
    (and 
      (<= 0 px)
      (< px mx)
      (<= 0 py)
      (< py my))))

(defn pt1-get-all-antinodes [task]
  (->> task
    :antennas
    vals
    (map antinodes-for-all)
    (reduce set/union)))

(defn pt1
  [task]
  (->> (pt1-get-all-antinodes task)
    (filter #(inside-dimensions? (:dimensions task) %))
    count))



(comment let [
              ;t (parse-input (slurp input-txt))
              t (parse-input sample)
              all (pt1-get-all-antinodes t)]
  (prn (:dimensions t))
  (doseq [x (range (inc (get-in t [:dimensions :x])))]
    (doseq [y (range (inc (get-in t [:dimensions :y])))]
      (print (if (all {:x x :y y}) "#" ".")))
    (print "\n"))
  )

(defn pt2
  [task]
  0)

(defn solve-1
  ([] (solve-1 (slurp input-txt)))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp input-txt)))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    14 (pt1 (parse-input sample))
    2 (pt2 (parse-input sample))))

(comment
  (solve-1)
  (solve-2))
