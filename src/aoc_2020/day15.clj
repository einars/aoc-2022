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
   (let [state (into {} (map vector start-stream (map vector (range 1 (inc (count start-stream))))))]
     (concat 
       start-stream 
       (streamline state (last start-stream) (inc (count start-stream))))))
  ([state last-number turn]
   (lazy-seq
     (let [new-number (if (= 1 (count (state last-number)))
                        0
                        (apply - (state last-number)))]
       (cons new-number
         (streamline 
           (if (contains? state new-number)
             (assoc state new-number [turn (first (state new-number))])
             (assoc state new-number [turn]))
           new-number
           (inc turn)))))))

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
     first
     )))

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
