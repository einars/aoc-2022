(ns aoc.core
  (:require
    [clojure.string :as str]
    [clojure.pprint :as pprint]
    )
  (:gen-class))

(defn solve
  [year day]
  (let [nsn (symbol (format "aoc-%d.day%d" year day))
        fns (try 
              (require nsn) 
              (ns-publics nsn)
              (catch Exception _ {}))
        solve-1 (fns 'solve-1)
        solve-2 (fns 'solve-2)]

    (when solve-1
      (printf "%d/%d part 1 = %s\n" year day (solve-1))
      (flush))
    (when solve-2
      (printf "%d/%d part 2 = %s\n" year day (solve-2)))))

(defn -main
  "Run all AOC tasks"
  [ & args]
  (if (seq args)
    (apply solve (->> (str/split (first args) #"/")
                   (map #(Integer/parseInt %))))
    (doseq [y [2023]
            d (range 32)]
      (solve y d))))
