(ns aoc-2023.day8
  (:require
    [clojure.test :as test :refer [deftest are]]
    [clojure.string :as str]
    [instaparse.core :as insta]
    [clojure.math.numeric-tower :refer [lcm]]
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

(def sample-ghost-map ["11A = (11B, XXX)"
                       "11B = (XXX, 11Z)"
                       "11Z = (11B, XXX)"
                       "22A = (22B, XXX)"
                       "22B = (22C, 22C)"
                       "22C = (22Z, 22Z)"
                       "22Z = (22B, 22B)"
                       "XXX = (XXX, XXX)"])

(def map-parser (insta/parser "
    <map> = name <' = ('> name <', '> name <')'>
    <name> = #'[\\dA-Z]+'
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


(defn cycle-length [start-pos path m]
  (let [terminal? (set (filter #(str/ends-with? % "Z") (keys m)))]
    ;(prn :start-pos start-pos terminal?)
    (loop [pos start-pos, steps 0, path (->> path (map str) (map keyword) cycle)]
      (if (terminal? pos)
        steps
        (recur 
          (get (pos m) (first path))
          (inc steps) (drop 1 path))))))

(defn parse-input [s]
  (let [[path m] (str/split s #"\n\n")]
    [path (parse-map m)]))

(def input-file "resources/2023/day8.txt")

(defn solve-1
  ([] (solve-1 (h/slurp-strings input-file)))
  ([[path _ & m]] (aaa-to-zzz path (parse-map m))))

(defn solve-2
  ([] (solve-2 (h/slurp-strings input-file)))
  ([[path _ & m]] 
   (let [m (parse-map m)
         start-poss (filter #(str/ends-with? % "A") (keys m))
         cycles (map #(cycle-length % path m) start-poss)]
     (reduce lcm cycles) )))

(comment
  (solve-1)
  (solve-2))

