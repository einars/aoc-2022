(ns aoc-2024.day10
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.tools.trace :refer :all]
   [instaparse.core :as insta]
   [aoc.helpers :as h]))

(def input-txt "resources/2024/day10.txt")

(def sample
  (str/trim "
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
  "))

(defn parse-input
  [s]
  (h/remap (h/string->map s) #(- (int %) 0x30)))

(def ^:dynamic *part* 1)

(defn fall-down 
  ([m positions expect]
   (let [npositions (apply concat
                      (for [p positions]
                        (filterv #(= (m %) expect) (h/neighbors-4 p))))

         npositions (if (= *part* 1) 
                      (set npositions) 
                      npositions)]

     (if (= 0 expect)
       npositions
       (recur m npositions (dec expect))))))

(defn add-to-start [accu pts]
  (reduce (fn [accu pt] (update accu pt #(inc (or % 0)))) accu pts))

(defn pt1
  [task]
  (let [heads (h/find-keys #{9} task)
        head-starting-points (mapv #(fall-down task #{%} 8) heads)]
    (->> head-starting-points
      (reduce add-to-start {})
      vals
      (reduce +))))

(pt1 (parse-input sample))

(binding [*part* 2]
  (pt1 (parse-input sample)))

(defn pt2
  [task]
  (binding [*part* 2] (pt1 task)))

(defn solve-1
  ([] (solve-1 (slurp input-txt)))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp input-txt)))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    36 (pt1 (parse-input sample))
    81 (pt2 (parse-input sample))))

(comment
  (solve-1)
  (solve-2))
