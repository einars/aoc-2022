(ns aoc-2023.day8
  (:require
    [clojure.test :as test :refer [deftest are]]
    [clojure.string :as str]
    [instaparse.core :as insta]
    [aoc.helpers :as h]))

(def sample-map-1 ["AAA = (BBB, CCC)"
                   "BBB = (DDD, EEE)"
                   "CCC = (ZZZ, GGG)"
                   "DDD = (DDD, DDD)"
                   "EEE = (EEE, EEE)"
                   "GGG = (GGG, GGG)"
                   "ZZZ = (ZZZ, ZZZ)"])

(def sample-map-2 ["AAA = (BBB, BBB)"
                   "BBB = (AAA, ZZZ)"
                   "ZZZ = (ZZZ, ZZZ)"])

(def map-parser (insta/parser "
    <map> = name <' = ('> name <', '> name <')'>
    <name> = #'[A-Z]+'
    <int> = #'\\d+'
    "))

(defn parse-map [lines]
  (into {} (map (fn [[n l r]] [(keyword n) {:L (keyword l) :R (keyword r)}]) (map map-parser lines))))

(defn aaa-to-zzz [path m]
  (loop [pos :AAA, steps 0, path (->> path (map str) (map keyword) cycle)]
    ;(prn :hey pos steps (first path))
    (if (= pos :ZZZ)
      steps
      (recur 
        (get (pos m) (first path))
        (inc steps) (drop 1 path)))))

(defn parse-input [s]
  (let [[path m] (str/split s #"\n\n")]
    [path (parse-map m)]))

(def input-file "resources/2023/day8.txt")

(defn solve-1
  ([] (solve-1 (h/slurp-strings input-file)))
  ([[path _ & m]] (aaa-to-zzz path (parse-map m))))

(defn solve-2
  ([] (solve-2 (h/slurp-strings input-file)))
  ([m] (->> m
         )))

(deftest test-stuff [] 
  (are [x y] (= x y)
    2 (aaa-to-zzz "RL" (parse-map sample-map-1))
    6 (aaa-to-zzz "LLR" (parse-map sample-map-2))
    ;0 (solve-2 sample-map)
    ))

(comment
  (solve-1)
  (solve-2))
