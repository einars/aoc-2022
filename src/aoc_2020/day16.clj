(ns aoc-2020.day16
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.core.logic :as logic :refer [everyg]]
    [clojure.core.logic.fd :as fd]
    [clojure.set :as set]
    [clojure.string :as string]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn parse-constraint [s]
  (let [[[_ name f1 t1 f2 t2]] (re-seq #"^([a-z ]+): (\d+)-(\d+) or (\d+)-(\d+)" s)
        f1 (Integer/parseInt f1)
        f2 (Integer/parseInt f2)
        t1 (Integer/parseInt t1)
        t2 (Integer/parseInt t2)]
    {:name name :test #(or (<= f1 % t1) (<= f2 % t2))}))

(defn parse [s]
  (let [[constraints s] (string/split s #"\n\nyour ticket:\n")
        [my-ticket nearby] (string/split s #"\n\nnearby tickets:\n")]
    {:constraints (map parse-constraint (string/split constraints #"\n"))
     :ticket (h/to-int-list my-ticket)
     :nearby (map h/to-int-list (string/split nearby #"\n"))}))

(defn possible-constraints [n constraints]
  (filter #((:test %) n) constraints))

(defn find-invalid-numbers
  "which numbers in :nearby tickets don't match any constraints?"
  [prob]
  (filter #(empty? (possible-constraints % (:constraints prob))) (reduce concat (:nearby prob))))

(defn solve-1
  ([] (solve-1 "resources/2020/day16.txt"))
  ([file]
   (->>
     (slurp file)
     (parse)
     (find-invalid-numbers)
     (reduce +))))


(deftest test-stuff [] 
  (test/are [x y] (= x y)
    71 (solve-1 "resources/2020/day16.test.txt")))
