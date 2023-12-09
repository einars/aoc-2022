(ns aoc-2023.day9
  (:require
    [clojure.test :as test :refer [deftest are]]
    [clojure.string :as str]
    [aoc.helpers :as h]))

(def sample-data ["0 3 6 9 12 15"
                  "1 3 6 10 15 21"
                  "10 13 16 21 30 45"])

(def input-file "resources/2023/day9.txt")

(defn as-int-list [s]
  (map parse-long (str/split s #" ")))

(defn count-solution [accu]
  (reduce + (map last accu)))

(defn predict 
  ([xs] (predict xs []))
  ([xs accu]
   (if (apply = xs)
     (count-solution (conj accu xs))
     (recur (map (fn [[a b]] (- b a)) (partition 2 1 xs)) (conj accu xs)))))

(predict [0 3 6 9 12 15])

(defn solve-1
  ([] (solve-1 (h/slurp-strings input-file)))
  ([m] (->> m
         )))

(defn solve-2
  ([] (solve-2 (h/slurp-strings input-file)))
  ([m] (->> m
         )))

(deftest test-stuff [] 
  (are [x y] (= x y)
    0 (solve-1 sample-data)
    0 (solve-2 sample-data)))

(comment
  (solve-1)
  (solve-2))
