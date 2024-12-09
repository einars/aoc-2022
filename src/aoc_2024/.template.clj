(ns aoc-2024.dayX
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [clojure.tools.trace :refer :all]
   [instaparse.core :as insta]
   [aoc.helpers :as h]))

(def input-txt "resources/2024/day6.txt")

(def sample
  "")

(defn parse-input
  [s]
  s)

(defn pt1
  [task]
  0)

(defn pt2
  [task]
  0)

(defn solve-1
  ([] (solve-1 (slurp input-txt)))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp input-txt)))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    1 (pt1 (parse-input sample))
    2 (pt2 (parse-input sample))))

(comment
  (solve-1)
  (solve-2))
