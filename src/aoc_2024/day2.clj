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

(defn safe-mode [[a b]]
  (condp = (- a b)
    1 :inc
    2 :inc
    3 :inc
    -1 :dec
    -2 :dec
    -3 :dec
    nil))

(defn safe-report? 
  [xs]
  (let [deltas (mapv safe-mode (partition 2 1 xs))]
    (and 
      (= 1 (count (set deltas)))
      (not (some nil? deltas)))))

(defn drop-nth
  [n xs]
  (concat (take n xs) (nthrest xs (inc n))))

(defn safe-report-pt2? 
  [xs]
  (let [variations (mapv #(drop-nth % xs) (range (count xs)))]
    (some safe-report? variations)))

(mapv safe-report-pt2? (parse-input sample))

(defn pt1
  [task]
  (count (filterv safe-report? task)))

(defn pt2
  [task]
  (count (filterv safe-report-pt2? task)))


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
    false (safe-report? [1 10 20])
    true (safe-report-pt2? [7 6 4 2 1])
    true (safe-report-pt2? [1 3 2 4 5])
    nil  (safe-report-pt2? [1 2 7 8 9])
    2 (pt1 (parse-input sample))
    4 (pt2 (parse-input sample))))

(comment
  (solve-1)
  (solve-2))
