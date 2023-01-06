(ns aoc-2020.day15
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.string :as str]))

(defn streamline 
  ([start-stream]
   (let [state (into {} (map vector start-stream (map inc (range (count start-stream)))))]
     (concat 
       start-stream
       (streamline (transient state) 0 (inc (count start-stream))))))
  ([state number turn]
   (lazy-seq
     (let [next-number (- turn (get state number turn))]
       (cons number 
         (streamline (assoc! state number turn) next-number (inc turn)))))))

(defn to-int-list [s]
  (map #(Integer/parseInt %) (str/split (str/trim s) #",")))

(defn solve-1
  ([] (solve-1 "resources/2020/day15.txt"))
  ([file] (solve-1 file 2020))
  ([file n]
   (->>
     (slurp file)
     to-int-list
     streamline
     (drop (dec n))
     first)))

(defn solve-2
  ([] (solve-1 "resources/2020/day15.txt" 30000000)))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    436 (solve-1 "resources/2020/day15.test.txt")))
