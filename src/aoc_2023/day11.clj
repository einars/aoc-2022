(ns aoc-2023.day11
  (:require
    [clojure.test :as test :refer [deftest are]]
    [aoc.helpers :as h]))

(def sample-data 
  ["...#......"
   ".......#.."
   "#........."
   ".........."
   "......#..."
   ".#........"
   ".........#"
   ".........."
   ".......#.."
   "#...#....."])

(def input-file "resources/2023/day11.txt")

(defn empty-lines [m]
  (let [{:keys [xmin xmax ymin ymax]} (h/map-dimensions m)
        xs (set (mapv :x (keys m)))
        ys (set (mapv :y (keys m)))]
    {:xs (remove xs (range xmin (inc xmax)))
     :ys (remove ys (range ymin (inc ymax))) } ) )

(def ^:dynamic *multiplier* 2)

(defn readjust [xy key adj]
  (update xy key (fn [v] (+ v (* (dec *multiplier*) (count (filter #(< % v) adj)))))))

(defn readjust-coords [m]
  (let [adj (empty-lines m)]
    (as-> m XXX
      (update-keys XXX #(readjust % :x (:xs adj)))
      (update-keys XXX #(readjust % :y (:ys adj))))))


(defn manhattan-dist [a b]
  (+
    (abs (- (:x a) (:x b)))
    (abs (- (:y a) (:y b)))) )

(defn score-1 [m]
  (/ (reduce + (for [g1 (keys m) g2 (keys m)]
                 (manhattan-dist g1 g2))) 2))

(defn solve-1
  ([] (solve-1 (first (h/slurp-xy-map input-file))))
  ([m] (->> m
         readjust-coords
         score-1)))

(defn solve-2
  ([] (solve-2 (first (h/slurp-xy-map input-file))))
  ([m] (binding [*multiplier* 1000000]
         (solve-1 m))))

(deftest test-stuff [] 
  (are [x y] (= x y)
    374 (solve-1 (first (h/make-xy-map sample-data)))
    374  (binding [*multiplier* 2] (solve-1 (first (h/make-xy-map sample-data))))
    1030 (binding [*multiplier* 10] (solve-1 (first (h/make-xy-map sample-data))))
    8410 (binding [*multiplier* 100] (solve-1 (first (h/make-xy-map sample-data))))))

(comment
  (solve-1)
  (solve-2))
