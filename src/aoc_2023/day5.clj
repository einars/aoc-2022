(ns aoc-2023.day5
  (:require
    [clojure.test :as test :refer [deftest are]]
    [clojure.string :as str]
    [instaparse.core :as insta]
    [aoc.helpers :as h]))

(def input-file "resources/2023/day5.txt")

(def sample-data "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(def sample-maps (drop 1 (str/split sample-data #"\n\n")))

(def map-parser (insta/parser "
    <map> = name <'-to-'> name <ws> <'map:'> <ws> ranges
    <ranges> = range (<ws> range)*
    range = int <ws> int <ws> int
    <name> = #'[a-z]+'
    <int> = #'\\d+'
    <ws> = (' ' | '\n' | '\r')+
    "))

(defn parse-seeds [s]
  (map parse-long (str/split (subs s 7) #" ")))

(defn parse-map [m]
  (let [parsed (map-parser m)
        [from to & ranges] parsed
        ranges (mapv #(map parse-long (drop 1 %)) ranges)]
    {:from (keyword from)
     :to (keyword to)
     :ranges ranges
     :intersects (merge 
                   (into {} (map (fn [[_ds ss len]] [(+ ss len) 0]) ranges))
                   (into {} (map (fn [[ds ss _len]] [ss (- ds ss)]) ranges)))}))

(defn find-map [k maps]
  (first (filter #(= (:from %) k) maps)))

(defn delta-at [m n]
  (if-let [idx (last (sort (filter #(<= % n) (keys (:intersects m)))))]
    (get-in m [:intersects idx])
    0))

(defn transform [m n]
  (+ n (delta-at m n)))

(defn next-intersect [m n]
  (first (sort (filter #(> % n) (keys (:intersects m))))))

(defn transform-range 
  [m [lo hi]] 

  (loop [n lo, accu []]
    (let [delta (delta-at m n)]
      (if-let [ix (next-intersect m n)]
        ; a) we have range
        (let [up-to (min (dec ix) hi)
              new-accu (conj accu [(+ delta n) (+ delta up-to)])]
          (if (= up-to hi)
            new-accu
            (recur (inc up-to) new-accu)))
        ; b ) finish
        (conj accu [(+ delta n) (+ delta hi)])))))


(defn grow [what n maps]
  (if (= :location what) n
    (let [m (find-map what maps)]
      (recur (:to m) (transform m n) maps))))

(defn grow-ranges [what ranges maps]
  (if (= :location what) ranges
    (let [m (find-map what maps)]
      (recur (:to m) (mapcat #(transform-range m %) ranges) maps))))

(defn parse-input [s]
  (let [[seeds & maps] (str/split s #"\n\n")
        seeds (parse-seeds seeds)
        maps (map parse-map maps)]
    [seeds maps]))

(defn parse-input-2 [s]
  (let [[seeds maps] (parse-input s)]
    [(mapv (fn [[a b]] [a (+ a b -1)]) (partition 2 seeds))
     maps]))

(defn solve-1
  ([] (solve-1 (slurp input-file)))
  ([m]  (let [[seeds maps] (parse-input m)]
          (->> seeds
            (mapv #(grow :seed % maps))
            (apply min)))))

(defn solve-2
  ([] (solve-2 (slurp input-file)))
  ([m]  (let [[seed-ranges maps] (parse-input-2 m)]
          (->> 
            (grow-ranges :seed seed-ranges maps)
            (mapv first)
            (apply min)))))

(deftest test-stuff [] 
  (are [x y] (= x y)
    82 (grow :seed 79 (mapv parse-map sample-maps))
    35 (solve-1 sample-data)
    46 (solve-2 sample-data)))

(comment
  (solve-1)
  (solve-2))
