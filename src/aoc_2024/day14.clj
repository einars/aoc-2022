(ns aoc-2024.day14
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [clojure.tools.trace :refer :all]
   [instaparse.core :as insta]
   [aoc.helpers :as h]))

(def input-txt "resources/2024/day14.txt")

(def ^:dynamic *part* 1)

(def ^:dynamic *bounds* [101 103])

(def sample
  "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
")

(def parser (insta/parser "
<robots> = robot (<nl> robot)* <nl>?
robot = <'p='> xy <' v='> xy
xy = int <','> int
int = #'-?\\d+'
nl = '\n'
  "))

(defn simplify-robot [[_ [_ x y] [_ vx vy]]]
  [x y vx vy])


(defn parse-input
  [s]
  (mapv simplify-robot (h/tree-parse-int (parser s))))

(defn walk-robot-step [[x y vx vy]]
  [ (mod (+ x vx) (first *bounds*))
   (mod (+ y vy) (second *bounds*))
   vx
   vy])

(defn walk-robot-times [[x y vx vy] times]
  [ (mod (+ x (* times vx)) (first *bounds*))
   (mod (+ y (* times vy)) (second *bounds*))
   vx
   vy])

(defn quadrant-of [[x y _ _]]
  (let [[bx by] *bounds*
        cutoff-x (/ (dec bx) 2)
        cutoff-y (/ (dec by) 2)]
    (cond
      (and
        (< -1 x cutoff-x)
        (< -1 y cutoff-y)) 1
      (and
        (< cutoff-x x bx)
        (< -1 y cutoff-y)) 2
      (and
        (< -1 x cutoff-x)
        (< cutoff-y y by)) 3
      (and
        (< cutoff-x x bx)
        (< cutoff-y y by)) 4
      )))

(defn print-map [m]
  (doseq [y (range 0 (second *bounds*))]
    (doseq [x (range 0 (first *bounds*))]
      (print (get m {:x x :y y} ".")))
    (print "\n")))


(defn print-robots [n robs]
  (print-map
    (reduce (fn [accu [x y]] (assoc accu {:x x :y y} "X")) {} robs))
  (println)
  (println (format "Iteration #%d" n)))

(defn quadrants [robots]
  (mapcat (fn [[k v]] 
            (when k [(count v)]))
    (group-by identity (mapv quadrant-of robots))))

(defn pt1
  [task]
  (reduce * 1 (quadrants (mapv #(walk-robot-times % 100) task))))

(defn run-print [robots]
  (let [[n quads robots]
        (->> [0 (quadrants robots) robots]
          (iterate (fn [[it quads rs]] 
                     (prn it)
                     (let [new-robots (mapv walk-robot-step rs)]
                       [(inc it) (reduce * 1 (quadrants new-robots)) new-robots])))
          (drop 1)
          (take 10000)
          (sort-by second)
          (first))]
    (print-robots n robots)))


(defn pt2
  [task]
  (run-print task))

(defn solve-1
  ([] (solve-1 (slurp input-txt)))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp input-txt)))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    12 (binding [*bounds* [11 7]] (pt1 (parse-input sample)))))

(comment
  (solve-1)
  (solve-2))

