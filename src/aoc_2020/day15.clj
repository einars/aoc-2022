(ns aoc-2020.day15
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn streamline 
  ([start-stream]
   (let [state (into {} (map vector start-stream (range 1 (inc (count start-stream)))))]
     (concat 
       start-stream
       (streamline state 0 (inc (count start-stream))))))
  ([state next-number turn]
   (lazy-seq
     (cons next-number 
       (streamline 
         (assoc state next-number turn)
         (if (contains? state next-number) (- turn (state next-number)) 0)
         (inc turn))))))

(defn to-int-list [s]
  (map #(Integer/parseInt %) (str/split (str/trim s) #",")))

(defn solve-1
  ([] (solve-1 "resources/2020/day15.txt"))
  ([file]
   (->>
     (slurp file)
     to-int-list
     streamline
     (drop 2019)
     first)))

(defn solve-2
  ([] (solve-2 "resources/2020/day15.txt"))
  ([file]
   (->>
     (slurp file)
     to-int-list
     streamline
     (drop 29999999)
     first)))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    436 (solve-1 "resources/2020/day15.test.txt")))
