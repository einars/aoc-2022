(ns aoc-2023.day15
  (:require
    [clojure.test :as test :refer [deftest are]]
    [clojure.string :as str]
    [aoc.helpers :as h]))

(def sample-data "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")
(def input-file "resources/2023/day15.txt")

(defn xhash [s]
  (reduce (fn [h n] (mod (* 17 (+ h n)) 256)) 0 (map int s)))

(defn solve-1
  ([] (solve-1 (str/trim (slurp input-file))))
  ([m] (->> (str/split m #",")
         (mapv xhash)
         (reduce +))))

(defn solve-2
  ([] (solve-2 (h/slurp-strings input-file)))
  ([m] (->> m
         )))

(deftest test-stuff [] 
  (are [x y] (= x y)
    52 (xhash "HASH") 
    1320 (solve-1 sample-data)
    0 (solve-2 sample-data)))

(comment
  (solve-1)
  (solve-2))
