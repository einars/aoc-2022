(ns aoc-2022.core
  (:require [aoc-2022.day1 :as day1])
  (:gen-class))

(def days 
  [[1 day1/solve-1 day1/solve-2]])

(defn -main
  "Run all AOC tasks"
  []
  (doseq [[d fn1 fn2] days]
    (printf "Day %d part 1 = %s\n" d (fn1))
    (when fn2
      (printf "Day %d part 2 = %s\n" d (fn2)))))
