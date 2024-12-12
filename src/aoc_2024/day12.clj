(ns aoc-2024.day12
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.tools.trace :refer :all]
   [instaparse.core :as insta]
   [aoc.helpers :as h]))

(def input-txt "resources/2024/day12.txt")

(def sample
  "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")

(defn parse-input
  [s]
  (h/string->map s))

(defn extract-region 
  [xy area]
  (loop [region #{xy}, area (disj area xy)]
    (let [nbs (set (filterv area (mapcat h/neighbors-4 region)))]
      (if (empty? nbs)
        [region area]
        (recur (set/union region nbs) (set/difference area nbs))))))

(defn extract-regions 
  [area]
  (loop [work-area (set area), regions []]
    (if (empty? work-area)
      regions
      (let [[region remaining-area] (extract-region (first work-area) work-area)]
        (recur remaining-area (conj regions region))))))

(defn split-area [area]
  (mapcat #(extract-regions (h/find-keys #{%} area)) (set (vals area))))

(defn area [region] (count region))

(defn perimeter
  [region] 
  (reduce +
    (for [c region]
      (h/count-where (complement region) (h/neighbors-4 c)))))

(defn count-sides
  [region]
  (+ 
    (count (extract-regions
             ;(filterv (fn [c] (not (region (h/top-of c)))) region)))
             (filterv (comp not region h/top-of)  region)))
    (count (extract-regions
             (filterv (fn [c] (not (region (h/bottom-of c)))) region)))
    (count (extract-regions
             (filterv (fn [c] (not (region (h/left-of c)))) region)))
    (count (extract-regions
             (filterv (fn [c] (not (region (h/right-of c)))) region)))))

(defn pt1
  [task]
  (reduce +
    (for [r (split-area task)]
      (* (area r) (perimeter r)))))

(defn pt2
  [task]
  (reduce +
    (for [r (split-area task)]
      (* (area r) (count-sides r)))))

(defn solve-1
  ([] (solve-1 (slurp input-txt)))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp input-txt)))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    1930 (pt1 (parse-input sample))
    1206 (pt2 (parse-input sample))
    ))

(comment
  (solve-1)
  (solve-2))
