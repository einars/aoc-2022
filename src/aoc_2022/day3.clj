(ns aoc-2022.day3
  (:require [clojure.test :as test :refer [deftest]])
  (:require [clojure.set :as set])
  (:require [aoc-2022.helpers :as h]))

(defn score-of 
  [c]
  (cond
    (<= (int \a) (int c) (int \z)) (+ 1 (- (int c) (int \a)))
    (<= (int \A) (int c) (int \Z)) (+ 27 (- (int c) (int \A)))
    :else nil))

(defn parse-rucksack
  [line]
  (let [[r1 r2] (split-at (/ (count line) 2) line)]
    [(set r1) (set r2)]))


(def file "resources/day3.test.txt")

(defn solve-single-line-1 
  [line]
  (let [[r1 r2] (parse-rucksack line)]
    (->>
      (set/intersection r1 r2)
      (map score-of)
      (reduce +))))

(defn solve-single-group-2
  [[l1 l2 l3]]
  (->>
    (set/intersection (set l1) (set l2) (set l3))
    (map score-of)
    (reduce +)))

(defn solve-1 
  ([] (solve-1 "resources/day3.txt"))
  ([file]
    (->>
      (h/slurp-strings file)
      (map solve-single-line-1)
      (reduce +))))
      

(defn solve-2
  ([] (solve-2 "resources/day3.txt"))
  ([file]
    (->>
      (h/slurp-strings file)
      (partition 3)
      (map solve-single-group-2)
      (reduce +))))


(deftest test-1 [] 
  (test/are [x y] (= x y)
    1 (score-of \a)
    26 (score-of \z)
    27 (score-of \A)
    52 (score-of \Z)
    [#{\a \b \c} #{\A \B \C}] (parse-rucksack "abcABC")
    157 (solve-1 "resources/day3.test.txt")
    70 (solve-2 "resources/day3.test.txt")
    ))


;(test/run-tests)
