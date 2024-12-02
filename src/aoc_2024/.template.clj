(ns aoc-2024.dayX
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [aoc.helpers :as h]))

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
  ([] (solve-1 (slurp "resources/2024/dayX.txt")))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp "resources/2024/dayX.txt")))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    1 (pt1 (parse-input sample))
    2 (pt2 (parse-input sample))))

(comment
  (solve-1)
  (solve-2))
