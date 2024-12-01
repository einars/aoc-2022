(ns aoc-2024.day1
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [aoc.helpers :as h]))

(def sample 
  "3   4
4   3
2   5
1   3
3   9
3   3")

(defn parse-input [s]
  (let [lines 
        (-> s
          str/trim
          (str/split #"\n"))
        numbers (mapv #(str/split % #" +") lines)
        ints (mapv (fn [[a b]] [(parse-long a) (parse-long b)]) numbers)]

    [(mapv first ints)
     (mapv second ints)]))

(defn pt1 [task]
  (let [[t1 t2] task
        s1 (sort t1)
        s2 (sort t2)]

    (reduce +(map (fn [a b] (abs (- a b))) (sort t1) (sort t2)) )) )

(defn times-in [xs x]
  (count (filter #(= % x) xs) ))

(defn pt2 [task]
  (let [[t1 t2] task]
    (reduce + (mapv (fn [a] (* a (times-in t2 a))) t1) )))

(defn solve-1 
  ([] (solve-1 (slurp "resources/2024/day1.txt")))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp "resources/2024/day1.txt")))
  ([ss] (pt2 (parse-input ss))))

;(defn solve-2 []
;  (top3-elves-carrying-the-most (h/slurp-strings "resources/2022/day1.txt")))

(deftest tests []
  (are [x y] (= x y)
    11 (pt1 (parse-input sample))
    31 (pt2 (parse-input sample))))

(comment
  (solve-1)
  (solve-2))
