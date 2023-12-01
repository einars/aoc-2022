(ns aoc-2023.day1
  (:require
    [clojure.test :as test :refer [deftest is are]]
    [aoc.helpers :as h]))

(def sample ["1abc2"
             "pqr3stu8vwx"
             "a1b2c3d4e5f"
             "treb7uchet"])

(def digits #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(defn char-to-int [c] (- (int c) (int \0)))

(defn leave-digits [l] (mapv char-to-int (filter digits l)))

(defn solve-1 
  ([] (solve-1 (h/slurp-strings "resources/2023/day1.txt")))
  ([ss]
   (let [digs (map leave-digits ss)]
     (reduce (fn [coll digs] (+ coll (* 10 (first digs)) (last digs))) 0 digs))))

;(defn solve-2 []
;  (top3-elves-carrying-the-most (h/slurp-strings "resources/2022/day1.txt")))

(deftest tests []
  (are [x y] (= x y)
    [1 2 3] (leave-digits "x1x2x3x")
    142 (solve-1 sample)


    )
  )
