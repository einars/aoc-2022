(ns aoc-2024.day2
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [aoc.helpers :as h]))

(def sample
  "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")


(defn parse-report
  [s]
  (mapv parse-long (str/split s #" ")))

(defn parse-input
  [s]
  (mapv parse-report (str/split s #"\n")))

(defn safe-report? 
  [xs]
  (let [deltas (mapv abs (mapv - (partition 2 1 xs)))]
    (= 1 (count (set deltas)))))

(defn pt1
  [task]
  )

(defn pt2
  [task]
  0)

(defn solve-1
  ([] (solve-1 (slurp "resources/2024/day2.txt")))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp "resources/2024/day2.txt")))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    true (safe-report? [7 6 4 2 1])
    false (safe-report? [1 2 7 8 9])
    1 (pt1 (parse-input sample))
    2 (pt2 (parse-input sample))))

(comment
  (solve-1)
  (solve-2))
