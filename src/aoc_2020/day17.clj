(ns aoc-2020.day17
  (:require
    [clojure.test :as test :refer [deftest]]
    [aoc.helpers :as h]))

(defrecord Coords [x y z w])

(defn make-4d-map [map-2d]
  (set (for [[{:keys [x y]} _] map-2d] 
         (Coords. x y 0 0))))

(defn neighbors-3d [{:keys [x y z w]}]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        dz [-1 0 1]] 
    (Coords. (+ dx x) (+ dy y) (+ dz z) w)))

(defn neighbors-4d [{:keys [x y z w]}]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        dz [-1 0 1]
        dw [-1 0 1]]
    (Coords. (+ dx x) (+ dy y) (+ dz z) (+ dw w))))

(def ^:dynamic *neighbors* neighbors-3d)

(defn append-neighbor-counts [m cube]
  (reduce (fn [m xyz] (update m xyz #(inc (or % 0)))) m (*neighbors* cube)))

(defn step [old-state]
  (->> old-state
    (reduce append-neighbor-counts {})
    (filter (fn [[xyz count]]
              (if (contains? old-state xyz) 
                (or (= count 3) (= count 4)); 2 or 3 neighbors, but self is already counted as well
                (= count 3))))
    (map first)
    set))

(defn solve-1
  ([] (solve-1 "resources/2020/day17.txt"))
  ([file]
   (->>
     (h/slurp-xy-map file)
     (first)
     (make-4d-map)
     (iterate step)
     (drop 6)
     first
     count)))

(defn solve-2
  ([] (solve-2 "resources/2020/day17.txt"))
  ([file]
   (binding [*neighbors* neighbors-4d]
     (solve-1 file))))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    112 (solve-1 "resources/2020/day17.test.txt")
    848 (solve-2 "resources/2020/day17.test.txt")))
