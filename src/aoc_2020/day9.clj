(ns aoc-2020.day9
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(def ^:dynamic *preamble-count* 25)

(defn matches-preamble? [xs]
  (let [preamble (set (take *preamble-count* xs))
        n (last xs)]
    (some #(and
             (not= (- n %) %)
             (contains? preamble (- n %))) preamble)))

(defn solve-1
  ([] (solve-1 "resources/2020/day9.txt"))
  ([file]
   (->>
     (h/slurp-ints file)
     (partition (inc *preamble-count*) 1 (repeat 0))
     (drop-while matches-preamble?)
     first
     last)))

(defn find-partition-summing-to 
  ([xs n] (find-partition-summing-to xs n 0))
  ([xs n idx-start]
   (loop [idx idx-start sum 0]
     (cond
       (= sum n) (->> xs (drop idx-start) (take (- idx idx-start)))
       (= idx (count xs)) (find-partition-summing-to xs n (inc idx-start))
       (> sum n) (find-partition-summing-to xs n (inc idx-start))
       :else (recur (inc idx) (+ sum (get xs idx)))))))

(defn solve-2
  ([] (solve-2 "resources/2020/day9.txt"))
  ([file]
   (let [numbers (vec (h/slurp-ints file))
         crap-number (->> numbers
                       (partition (inc *preamble-count*) 1 (repeat 0))
                       (drop-while matches-preamble?)
                       first
                       last)
         partition (find-partition-summing-to numbers crap-number)
         ]
     (+ (apply min partition) (apply max partition)))))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    127 (binding [*preamble-count* 5] (solve-1 "resources/2020/day9.test.txt"))
    62 (binding [*preamble-count* 5] (solve-2 "resources/2020/day9.test.txt"))))
