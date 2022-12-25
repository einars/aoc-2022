(ns aoc-2022.day25
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

;(defn int->snafu [n] nil)

(def defu-chars {\= -2, \- -1, \0 0, \1 1, \2 2})
(def snafu-chars {0 \=, 1 \- 2, \0 3, \1 4, \2})

(defn snafu->int 
  ([s] (snafu->int (reverse s) 1 0))
  ([rs n accu]
   (if (seq rs)
     (recur (rest rs) (* n 5) (+ accu (* n (defu-chars (first rs)))))
     accu )))


(defn int->snafu 
  ([n] (int->snafu n 0 []))
  ([n adj accu]
   (let [qq (quot (+ n adj) 5)
         rr (mod (+ n adj 2) 5)
         new-accu (conj accu (snafu-chars rr))
         new-adj (if (< rr 2) 1 0)]
     (if (and (zero? qq) (zero? new-adj))
       (str/join (reverse new-accu))
       (recur qq new-adj new-accu)))))

(defn solve-1
  ([] (solve-1 "resources/2022/day25.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (map snafu->int)
     (reduce +)
     int->snafu)))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    "2=-1=0" (solve-1 "resources/2022/day25.test.txt")
    1747 (snafu->int "1=-0-2")
    "1121-1110-1=0" (int->snafu 314159265)
    ))

