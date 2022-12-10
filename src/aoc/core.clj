(ns aoc.core
  (:require
    [clojure.string :as str]
    [clojure.pprint :as pprint]

    [aoc-2016.day1]
    [aoc-2016.day2]

    [aoc-2022.day1]
    [aoc-2022.day2]
    [aoc-2022.day3]
    [aoc-2022.day4]
    [aoc-2022.day5]
    [aoc-2022.day6]
    [aoc-2022.day7]
    [aoc-2022.day8]
    [aoc-2022.day9]
    [aoc-2022.day10])
  (:gen-class))

(defn solve
  [year day]
  (let [nsn (symbol (format "aoc-%d.day%d" year day))
        fns (try (ns-publics nsn) (catch Exception _ {}))
        solve-1 (fns 'solve-1)
        solve-2 (fns 'solve-2)]

    (when solve-1
      (printf "%d/%d part 1 = %s" year day (with-out-str (pprint/pprint (solve-1)))))
    (when solve-2
      (printf "%d/%d part 2 = %s" year day (with-out-str (pprint/pprint (solve-2)))))))

(defn -main
  "Run all AOC tasks"
  [ & args]
  (if (seq args)
    (apply solve (->> (str/split (first args) #"/")
                      (map #(Integer/parseInt %))))
    (doseq [y (range 2015 2023)
            d (range 26)]
      (solve y d))))
