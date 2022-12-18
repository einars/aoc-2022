(ns aoc-2022.day18
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn parse-xyz [s]
  (map #(Integer/parseInt %) (str/split s #",")))

(defn immediate-neighbors [[x y z]] 
  [[(inc x) y z]
   [(dec x) y z]
   [x (inc y) z]
   [x (dec y) z]
   [x y (inc z)]
   [x y (dec z)]])

(defn neighbor-cube 
  [pt] 
  (for [dx (range -1 2)
        dy (range -1 2)
        dz (range -1 2)]
    (mapv + pt [dx dy dz])))


(defn count-open-sides 
  [lava c]
  (apply + (map #(if (lava %) 0 1) (immediate-neighbors c))))

(defn count-crusty-sides 
  [crust c]
  (apply + (map #(if (crust %) 1 0) (immediate-neighbors c))))

(defn count-all-open-sides [lava]
  (reduce + (map #(count-open-sides lava %) lava)))

(defn count-all-crusty-sides [lava crust]
  (reduce + (map #(count-crusty-sides crust %) lava)))

(defn build-crust 
  [first-crust lava]

  (loop [accu #{first-crust} pool #{first-crust}]
    (let [new-pool (->> pool
                     (mapcat immediate-neighbors)
                     (filter (complement accu)) ; not-yet seen
                     (filter (complement pool)) ; not in current pool
                     (filter (complement lava)) ; not lava
                     (filter #(some lava (neighbor-cube %)))
                     set)]
      (if (empty? new-pool)
        accu
        (recur (reduce conj accu pool) new-pool)))))

(defn ext-surface-area [lava]
  (let [leftmost (first (sort-by first lava))
        first-crust (mapv - leftmost [1 0 0])
        crust (build-crust first-crust lava)]
    (count-all-crusty-sides lava crust)))

(defn solve-1
  ([] (solve-1 "resources/2022/day18.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (map parse-xyz)
     (set)
     (count-all-open-sides))))

(defn solve-2
  ([] (solve-2 "resources/2022/day18.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (map parse-xyz)
     (set)
     (ext-surface-area))))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    10 (count-all-open-sides #{[1 1 1] [2 1 1]})
    64 (solve-1 "resources/2022/day18.test.txt")
    58 (solve-2 "resources/2022/day18.test.txt")))
