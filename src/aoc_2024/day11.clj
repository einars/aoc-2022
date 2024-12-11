(ns aoc-2024.day11
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [clojure.math :as math]
   [clojure.tools.trace :refer :all]
   [instaparse.core :as insta]
   [aoc.helpers :as h]))

(def input-txt "resources/2024/day11.txt")

(defn parse-input
  [s]
  (mapv parse-long (str/split (str/trim s) #" ")))

(defn split-stone [s]
  (let [n-digits (inc (int (math/log10 s)))
        mask (reduce * (repeat (/ n-digits 2) 10))]
    [(quot s mask) (rem s mask)]))

(def get-length)

(defn get-length-impl [n generations]
  (cond
    (= 0 generations ) 1
    (= 0 n) (get-length 1 (dec generations))

    (even? (inc (int (math/log10 n))))
    (let [[a b] (split-stone n)]
      (+ (get-length a (dec generations))
        (get-length b (dec generations))))

    :else
    (get-length (* 2024 n) (dec generations))))

(def get-length (memoize get-length-impl))

(defn pt1
  [task]
  (reduce + (map #(get-length % 25) task)))

(defn pt2
  [task]
  (reduce + (map #(get-length % 75) task)))


(defn solve-1
  ([] (solve-1 (slurp input-txt)))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp input-txt)))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    55312 (pt1 [125 17])
    2 (pt2 (parse-input sample))))

(comment
  (solve-1)
  (solve-2))
