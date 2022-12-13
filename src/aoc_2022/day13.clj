(ns aoc-2022.day13
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.edn :as edn]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [aoc.helpers :as h]))

(defn compare-lists [p1 p2]
  (cond
    (and (number? p1) (number? p2)) (- (compare p1 p2)) ; sic
    (and (seqable? p1) (number? p2)) (compare-lists p1 [p2])
    (and (number? p1) (seqable? p2)) (compare-lists [p1] p2)
    :else (loop [[pt1 & rest1] p1 [pt2 & rest2] p2]
      (cond
        (and (nil? pt1) (nil? pt2)) 0
        (nil? pt1) 1 ; ran out of left
        (nil? pt2) -1 ; ran out of right
        :else (let [comparison (compare-lists pt1 pt2)]
          (if (zero? comparison)
            (recur rest1 rest2)
            comparison))))))


(defn right-order? [p1 p2] (= 1 (compare-lists p1 p2)))

(defn solve-1
  ([] (solve-1 "resources/2022/day13.txt"))
  ([file]
   (let [pairs (->> (str/split (slurp file) #"\n\n")
                 (map #(str/split % #"\n"))
                 (map (fn [pairs] (map edn/read-string pairs))))
         indexed-pairs (h/zip pairs (iterate inc 1))]
     (->> indexed-pairs
       (filter (fn [[[p1 p2] _idx]] (right-order? p1 p2)))
       (map second) ; get index
       (reduce +)))))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    true (right-order? [1 1 3 1 1] [1 1 5 1 1])
    true (right-order? [[1],[2,3,4]] [[1],4])
    true (right-order? [[4,4],4,4] [[4,4],4,4,4])
    false (right-order? [[[]]] [[]])
    13 (solve-1 "resources/2022/day13.test.txt")))
