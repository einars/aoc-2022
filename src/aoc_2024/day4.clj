(ns aoc-2024.day4
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.tools.trace :refer :all]
   [clojure.string :as str]
   [aoc.helpers :as h]))

(def sample
  (str/trim "
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
    "))

(defn all-diag-2 [[lines n]]
  (for [x (range 0 (+ n n))]
    (apply str
      (for [y (range n)]
        (get-in lines [(- x y) y])))))

(defn all-diag-1 [[lines n]]
  (for [x (range (- n) n)]
    (apply str
      (for [y (range n)]
        (get-in lines [(+ x y) y])))))

(defn all-horiz [[lines n]]
  (for [x (range n)]
    (apply str
      (for [y (range n)]
        (get-in lines [x y])))))

(defn all-vert [[lines n]]
  (for [x (range n)]
    (apply str
      (for [y (range n)]
        (get-in lines [y x])))))


(defn parse-input
  [s]
  (let [lines (str/split s #"\n")]
    [lines (count (first lines))]))

(defn count-word 
  ([s val] (count-word s val 0 0))
  ([s val idx accum] 
   (if-let [idx (str/index-of s val idx)]
     (recur s val (inc idx) (inc accum))
     accum)))

(defn count-xmas [line]
  (+ (count-word line "XMAS") (count-word line "SAMX")))


(defn pt1 [task]
  (+
    (apply + (map count-xmas (all-horiz task)))
    (apply + (map count-xmas (all-vert task)))
    (apply + (map count-xmas (all-diag-1 task)))
    (apply + (map count-xmas (all-diag-2 task)))))

(def is-mas? #{"MASMAS" "MASSAM" "SAMMAS" "SAMSAM"})

(defn pt2 [[lines n]]
  (count 
    (filter is-mas? 
      (flatten
        (for [x (range 1 (dec n))]
          (for [y (range 1 (dec n))]
            (format "%s%s%s%s%s%s"
              (get-in lines [(dec x) (dec y)])
              (get-in lines [x y])
              (get-in lines [(inc x) (inc y)])

              (get-in lines [(dec x) (inc y)])
              (get-in lines [x y])
              (get-in lines [(inc x) (dec y)]))))))))

(defn solve-1
  ([] (solve-1 (slurp "resources/2024/day4.txt")))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp "resources/2024/day4.txt")))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    18 (pt1 (parse-input sample))
    9 (pt2 (parse-input sample))))

(comment
  (solve-1)
  (solve-2))
