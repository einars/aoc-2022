(ns aoc-2020.day10
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn get-answer [diffs]
  (let [diffs (conj diffs 3)
        grouped (group-by identity diffs)]
    (* (count (get grouped 3 0)) (count (get grouped 1 0)))))


(def arrangements)

(defn arrangements-impl [n xs]
  (let [res (+ 
              (if (xs (+ n 1)) (arrangements (+ n 1) xs) 0)
              (if (xs (+ n 2)) (arrangements (+ n 2) xs) 0)
              (if (xs (+ n 3)) (arrangements (+ n 3) xs) 0))]
    (if (= 0 res) 1 res)))

(def arrangements (memoize arrangements-impl))

(defn add-0 [xs] (conj xs 0))

(defn solve-1
  ([] (solve-1 "resources/2020/day10.txt"))
  ([file]
   (->>
     (h/slurp-ints file)
     add-0
     (sort >)
     (partition 2 1)
     (map (partial apply -))
     get-answer)))

(defn solve-2
  ([] (solve-2 "resources/2020/day10.txt"))
  ([file]
   (->>
     (h/slurp-ints file)
     add-0
     set
     (arrangements 0))))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    35 (solve-1 "resources/2020/day10.test.txt")
    220 (solve-1 "resources/2020/day10.test2.txt")
    19208 (solve-2 "resources/2020/day10.test2.txt")))
