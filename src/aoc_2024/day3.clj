(ns aoc-2024.day3
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [instaparse.core :as insta]
   [aoc.helpers :as h]))

(def sample
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(def sample-2
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")


(defn parse-input
  [s]
  (mapv (fn [[_ a b]]
          [(parse-long a) (parse-long b)])
    (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)" s)))

(defn filter-pt-2
  ([xs] 
   (filter-pt-2 xs [] true))
  ([[[cmd x y] & rest] accu do?]
   (if (nil? cmd)
     accu

     (condp = cmd
       :do (recur rest accu true)
       :dont (recur rest accu false)
       :mul (recur rest 
              (if do? (conj accu (* x y)) accu)
              do?)))))

(defn parse-input-2
  [s]
  (mapv (fn [[x a b]]
          (cond
            (= x "do()") [:do]
            (= x "don't()") [:dont]
            :else [:mul (parse-long a) (parse-long b)]))
    (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)" s)))

(defn pt1
  [task]
  (reduce + (mapv (fn [[a b]] (* a b)) task)))

(defn pt2
  [task]
  (reduce + (filter-pt-2 task)))

(defn solve-1
  ([] (solve-1 (slurp "resources/2024/day3.txt")))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp "resources/2024/day3.txt")))
  ([ss] (pt2 (parse-input-2 ss))))

(deftest tests []
  (are [x y] (= x y)
    161 (pt1 (parse-input sample))
    48 (pt2 (parse-input-2 sample-2))))

(comment
  (solve-1)
  (solve-2))
