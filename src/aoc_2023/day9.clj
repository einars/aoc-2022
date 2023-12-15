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

(defn count-solution-1 [accu]
  (reduce + (map last accu)))

(defn count-solution-2
  ([accu] (count-solution-2 (reverse accu) 0))
  ([accu sol]
   (if (seq accu)
     (recur (next accu) (- (first (first accu)) sol))
     sol)))

(def ^:dynamic *count-op* count-solution-1)

(defn predict 
  ([xs] (predict xs []))
  ([xs accu]
   (if (apply = xs)
     (*count-op* (conj accu xs))
     (recur (map (fn [[a b]] (- b a)) (partition 2 1 xs)) (conj accu xs)))))

(binding [*count-op* identity]
  (predict [0 3 6 9 12 15])
  )

(defn solve-1
  ([] (solve-1 (h/slurp-strings input-file)))
  ([m] (->> m 
         (map as-int-list)
         (map predict)
         (reduce +))))


(defn solve-2
  ([] (solve-2 (h/slurp-strings input-file)))
  ([m] (binding [*count-op* count-solution-2] 
         (solve-1 m))))

(deftest test-stuff [] 
  (are [x y] (= x y)
    18 (predict [0 3 6 9 12 15])
    -3 (binding [*count-op* count-solution-2] (predict [0 3 6 9 12 15]))
    5 (binding [*count-op* count-solution-2] (predict [10 13 16 21 30 45]))
    114 (solve-1 sample-data)
    2 (solve-2 sample-data)
    ))

(comment
  (solve-1)
  (solve-2))
