(ns aoc-2022.day1
  (:require [clojure.test :as test :refer [deftest is]])
  (:require [aoc-2022.helpers :as h]))


(defn elf-carrying-most-calories
  [lines]
  (loop [lines lines, accum []]
    (let [[first rest] (split-with #(not= "" %) lines)]
      (if (seq first)
        (recur (drop 1 rest) (conj accum (reduce + (mapv #(Integer/parseInt %) first))))
        (apply max accum)))))

;(elf-carrying-most-calories (h/slurp-strings "resources/day1.test.txt"))


(defn solve-with [f]
  (elf-carrying-most-calories (h/slurp-strings f)))

(defn solve []
  (solve-with "resources/day1.txt"))

(deftest self-test []
  (is (= (elf-carrying-most-calories (h/slurp-strings "resources/day1.test.txt")) 24000)))


;(test/run-tests)
