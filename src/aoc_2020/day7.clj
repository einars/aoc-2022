(ns aoc-2020.day7
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn make-kw [s]
  (keyword (str/replace s " " "-")))

(defn parse-rest [s] 
  (if (= "no other bags" s)
    {}
    (into {} (map (fn [[_ n t]] 
                    [(make-kw t) (Integer/parseInt n)])
               (re-seq #"(\d+) ([a-z ]+) bags?" s)))))

(defn parse [s]
  (if-let [[_ name rest] (first (re-seq #"^([a-z ]+) bags contain (.*)\.$" s))]
    [(make-kw name)
     (parse-rest rest)]
    (throw (Exception. (format "unable to parse: [%s]" s)))))

(defn count-type
  ([t bags] (count (disj (count-type t bags #{}) t)))
  ([t bags accu]
   (reduce 
     set/union 
     (if (bags t) (conj accu t) accu)
     (for [[bag-type bagses] bags :when (bagses t)] (count-type bag-type bags #{})))))

(defn count-bagrev
  [t bags]
  (reduce + 
    (if (bags t) 1 0)
    (map (fn [[bag-type n-bags]] (* n-bags (count-bagrev bag-type bags))) (bags t))))


(defn solve-1
  ([] (solve-1 "resources/2020/day7.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (map parse)
     (into {})
     (count-type :shiny-gold))))

(defn solve-2
  ([] (solve-2 "resources/2020/day7.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (map parse)
     (into {})
     (count-bagrev :shiny-gold)
     dec)))


(deftest test-stuff [] 
  (test/are [x y] (= x y)
    4 (solve-1 "resources/2020/day7.test.txt")
    32 (solve-2 "resources/2020/day7.test.txt")
    126 (solve-2 "resources/2020/day7.test2.txt")))
