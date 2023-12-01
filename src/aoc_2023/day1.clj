(ns aoc-2023.day1
  (:require
    [clojure.test :as test :refer [deftest is are]]
    [clojure.string :as str]
    [aoc.helpers :as h]))

(def sample ["1abc2"
             "pqr3stu8vwx"
             "a1b2c3d4e5f"
             "treb7uchet"])

(def sample-2 ["two1nine"
               "eightwothree"
               "abcone2threexyz"
               "xtwone3four"
               "4nineeightseven2"
               "zoneight234"
               "7pqrstsixteen" ])

(def digits #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(defn char-to-int [c] (- (int c) (int \0)))

(defn leave-digits [l] (mapv char-to-int (filter digits l)))

(def patterns {"one" "1"
               "two" "2"
               "three" "3"
               "four" "4"
               "five" "5"
               "six" "6"
               "seven" "7"
               "eight" "8"
               "nine" "9"})

(defn first-digit [l] 
  (let [pattern #"one|two|three|four|five|six|seven|eight|nine|ten"]
    (first (leave-digits (str/replace l pattern patterns)))))

(defn last-digit [l] 
  (let [pattern #"eno|owt|eerht|ruof|evif|xis|neves|thgie|enin|net"]
    (first (leave-digits (str/replace (str/reverse l) pattern #(patterns (str/reverse %)))))))

(map last-digit sample-2)

(defn solve-1 
  ([] (solve-1 (h/slurp-strings "resources/2023/day1.txt")))
  ([ss]
   (let [digs (map leave-digits ss)]
     (reduce (fn [coll digs] (+ coll (* 10 (first digs)) (last digs))) 0 digs))))


(defn solve-2
  ([] (solve-2 (h/slurp-strings "resources/2023/day1.txt")))
  ([ss]
   (reduce (fn [coll digs] (+ coll (* 10 (first-digit digs)) (last-digit digs))) 0 ss)))

;(defn solve-2 []
;  (top3-elves-carrying-the-most (h/slurp-strings "resources/2022/day1.txt")))

(deftest tests []
  (are [x y] (= x y)
    [1 2 3] (leave-digits "x1x2x3x")
    142 (solve-1 sample)
    281 (solve-2 sample-2)


    )
  )

(comment
  (solve-1)
  (solve-2))
