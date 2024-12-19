(ns aoc-2024.day19
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [clojure.tools.trace :refer :all]
   [instaparse.core :as insta]
   [aoc.helpers :as h]))

(def input-txt "resources/2024/day19.txt")

(def sample
  "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb")

(defn parse-input
  [s]
  (let [[towels, patterns] (str/split (str/trim s) #"\n\n")
        towels (str/split towels #", ")
        patterns (str/split patterns #"\n")]
    [towels patterns]))

(def pattern-possible?)
(defn pattern-possible-impl? [pattern towels]
  (if (= "" pattern) true
    (some identity (for [towel (filter (partial str/starts-with? pattern) towels)]
                     (pattern-possible? (subs pattern (count towel)) towels)))))
(def pattern-possible? (memoize pattern-possible-impl?))

(defn pt1
  [[towels patterns]]
  (count (filter #(pattern-possible? % towels) patterns)))


(def n-possibilities)
(defn n-possibilities-impl [pattern towels]
  (if (= "" pattern) 1
    (reduce + (for [towel (filter (partial str/starts-with? pattern) towels)]
                (n-possibilities (subs pattern (count towel)) towels)))))
(def n-possibilities (memoize n-possibilities-impl))

(defn pt2
  [[towels patterns]]
  (reduce + (map #(n-possibilities % towels) patterns)))

(defn solve-1
  ([] (solve-1 (slurp input-txt)))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp input-txt)))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    6 (pt1 (parse-input sample))
    16 (pt2 (parse-input sample))))

(comment
  (solve-1)
  (solve-2))
