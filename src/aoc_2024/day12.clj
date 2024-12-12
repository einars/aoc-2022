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
  [[xy ch] area]
  (loop [region #{xy}, area (dissoc area xy)]
    (let [nbs (set (flatten (mapv h/neighbors-4 region)))
          nbs (set/select #(= ch (get area %)) nbs)]

      (if (empty? nbs)
        [region area ]
        (recur (set/union region nbs) (reduce dissoc area nbs))))))

(defn extract-regions 
  [area]
  (loop [work-area area, regions []]
    (if (empty? work-area)
      regions
      (let [[region remaining-area] (extract-region (first work-area) work-area)]
        (recur remaining-area (conj regions region))))))

(defn area [region] (count region))

(defn perimeter
  [region] 
  (apply +
    (for [c region]
      (count (filter (complement region) (h/neighbors-4 c))))))

(defn extract-horizontal-side 
  [xy span]
  (loop [side #{xy}, span (disj span xy)]
    (let [nbs (set (flatten (map h/neighbors-h side)))
          nbs (set/select span nbs)]
      (if (empty? nbs)
        [side span]
        (recur (set/union side nbs) (set/difference span nbs))))))

(defn extract-vertical-side 
  [xy span]
  (loop [side #{xy}, span (disj span xy)]
    (let [nbs (set (flatten (map h/neighbors-v side)))
          nbs (set/select span nbs)]
      (if (empty? nbs)
        [side span]
        (recur (set/union side nbs) (set/difference span nbs))))))

(defn extract-horizontal-sides
  [spans]
  (loop [spans (set spans), accu []]
    (if (empty? spans)
      accu
      (let [[side remaining-spans] (extract-horizontal-side (first spans) spans)]
        (recur remaining-spans (conj accu side))))))

(defn extract-vertical-sides
  [spans]
  (loop [spans (set spans), accu []]
    (if (empty? spans)
      accu
      (let [[side remaining-spans] (extract-vertical-side (first spans) spans)]
        (recur remaining-spans (conj accu side))))))

(defn count-sides
  [region]
  (+ 
    (count (extract-horizontal-sides
             (filter (fn [c] (not (region (h/top-of c)))) region)))
    (count (extract-horizontal-sides
             (filter (fn [c] (not (region (h/bottom-of c)))) region)))
    (count (extract-vertical-sides
             (filter (fn [c] (not (region (h/left-of c)))) region)))
    (count (extract-vertical-sides
             (filter (fn [c] (not (region (h/right-of c)))) region)))))

(defn pt1
  [task]
  (apply +
    (for [r (extract-regions task)]
      (* (area r) (perimeter r)))))

(defn pt2
  [task]
  (apply +
    (for [r (extract-regions task)]
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
    1206 (pt2 (parse-input sample))))

(comment
  (solve-1)
  (solve-2))
