(ns aoc-2022.day1
  (:require [clojure.test :as test :refer [deftest is]])
  (:require [aoc.helpers :as h]))

(defn elves-carrying
  [lines]
  (loop [lines lines, accum []]
    (let [[first rest] (split-with #(not= "" %) lines)]
      (if (seq first)
        (recur (drop 1 rest) (conj accum (reduce + (mapv #(Integer/parseInt %) first))))
        accum))))

(defn elf-carrying-the-most
  [lines]
  (apply max (elves-carrying lines)))

(defn top3-elves-carrying-the-most
  [lines]
  (apply + (take 3 (sort > (elves-carrying lines)))))


;(elf-carrying-most-calories (h/slurp-strings "resources/2022/day1.test.txt"))

(defn solve-1 []
  (elf-carrying-the-most (h/slurp-strings "resources/2022/day1.txt")))

(defn solve-2 []
  (top3-elves-carrying-the-most (h/slurp-strings "resources/2022/day1.txt")))

(deftest self-test-1 []
  (is (= (elf-carrying-the-most (h/slurp-strings "resources/2022/day1.test.txt")) 24000)))

(deftest self-test-2 []
  (is (= (top3-elves-carrying-the-most (h/slurp-strings "resources/2022/day1.test.txt")) 45000)))

