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
        antennas (into {} 
                   (mapv (fn [ant] 
                           [ant 
                            (h/find-keys #(= ant %) area)])
                     antenna-types))]

    {:dimensions (second m)
     :antenna-types antenna-types
     :antennas antennas}))

(defn antinodes-for-pt1
  [a1 a2 _dim]
  (let [{x1 :x, y1 :y} a1
        {x2 :x, y2 :y} a2
        dx (- x2 x1)
        dy (- y2 y1)]

    [{:x (- x1 dx)
      :y (- y1 dy)}
     {:x (+ x2 dx)
      :y (+ y2 dy)}]))

(defn antinodes-for-pt2
  [a1 a2 dim]
  (let [{x1 :x, y1 :y} a1
        {x2 :x, y2 :y} a2
        {mx :x, my :y} dim
        dx (- x2 x1)
        dy (- y2 y1)]

    (concat
      [a1 a2]
      (loop [accu [] x (- x1 dx) y (- y1 dy)]
        (if (and (>= x 0) (>= y 0) (< x mx) (< y my))
          (recur (conj accu {:x x, :y y}) (- x dx) (- y dy))
          accu))
      (loop [accu [] x (+ x2 dx) y (+ y2 dy)]
        (if (and (>= x 0) (>= y 0) (< x mx) (< y my))
          (recur (conj accu {:x x, :y y}) (+ x dx) (+ y dy))
          accu)))))


(def ^:dynamic *antinode-fn* antinodes-for-pt1)

(defn inside-dimensions?
  [dims pt]
  (let [{mx :x my :y} dims
        {px :x py :y} pt]
    (and 
      (<= 0 px)
      (< px mx)
      (<= 0 py)
      (< py my))))


(defn antinodes-for-all [antennas dimensions]
  (->> (combo/combinations antennas 2)
    (map (fn [[a1 a2]] (*antinode-fn* a1 a2 dimensions)))
    flatten
    (filter (partial inside-dimensions? dimensions))
    set))



(defn get-all-antinodes [task]
  (->> task
    :antennas
    vals
    (mapv #(antinodes-for-all % (:dimensions task)))
    (reduce set/union)))

(defn pt1
  [task]
  (count (get-all-antinodes task)))


(defn pt2
  [task]
  (binding [*antinode-fn* antinodes-for-pt2]
    (count (get-all-antinodes task))))

(comment let [t (parse-input sample)
              all (binding [*antinode-fn* antinodes-for-pt2] (get-all-antinodes t))]
  (prn (:dimensions t))
  (doseq [y (range (get-in t [:dimensions :y]))]
    (doseq [x (range (get-in t [:dimensions :x]))]
      (print (if (all {:x x :y y}) "#" ".")))
    (print "\n"))
  )

(defn solve-1
  ([] (solve-1 (slurp input-txt)))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp input-txt)))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    14 (pt1 (parse-input sample))
    34 (pt2 (parse-input sample))))

(comment
  (solve-1)
  (solve-2))
