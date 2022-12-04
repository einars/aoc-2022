(ns aoc-2022.core
  (:require [aoc-2022.day1])
  (:require [aoc-2022.day2])
  (:require [aoc-2022.day3])
  (:require [aoc-2022.day4])
  (:gen-class))

(defn -main
  "Run all AOC tasks"
  []
  (doseq [d (range 25)]
    (let [nsn (symbol (str "aoc-2022.day" d))
      fns (try (ns-publics nsn) (catch Exception _ {}))
      solve-1 (fns 'solve-1)
      solve-2 (fns 'solve-2)]

      (when solve-1
        (printf "Day %d part 1 = %s\n" d (solve-1)))
      (when solve-2
        (printf "Day %d part 2 = %s\n" d (solve-2))))))
