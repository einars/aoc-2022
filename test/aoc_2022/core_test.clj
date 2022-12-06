(ns aoc-2022.core-test
  (:require [clojure.test :refer [run-tests]])
  (:require [aoc-2022.day1])
  (:require [aoc-2022.day2])
  (:require [aoc-2022.day3])
  (:require [aoc-2022.day4])
  (:require [aoc-2022.day5])
  (:require [aoc-2022.day6])
  )

(doseq [d (range 25)]
  (let [nsn (symbol (str "aoc-2022.day" d))]
    (try 
      (run-tests nsn)
      (catch Exception _ {}))))
