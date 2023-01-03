(ns aoc-2020.day13
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn parse-schedule [sched]
  (->> (str/split sched #",")
    (map #(when (not= "x" %) (Integer/parseInt %)))))

(defn parse [[n sched]]
  [(Integer/parseInt n) (parse-schedule sched)])

(defn get-wait-times [[n sched]]
  (map (fn [bus] [bus (- bus (mod n bus))]) (keep identity sched)))

(defn x-mod-n 
  ; find first x so that there exists x * n = rem (mod q)
  ([n q rem] (x-mod-n n q rem 1))
  ([n q rem x] 
   (cond 
     (= (mod (* x n) q) rem) x
     :else (recur n q rem (inc x)))))

(defn mod-solve 
  "
  a) X = a * n + b
  b) X = -p (mod q) 
  "
  [[a b] [p q]]
  (let [np (mod (- 0 p b) q)
        mult (x-mod-n a q np)]
    ; a * n = - p - b (mod q)
    [(* q a) (+ b (* mult a))]))

(defn s2 [xs]
  (let [[_ n] (first xs)
        xs (next xs)
        xs (filter #(some? (second %)) xs)]
    (second (reduce mod-solve [n 0] xs))))

(defn solve-1
  ([] (solve-1 "resources/2020/day13.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     parse
     get-wait-times
     (sort-by second)
     first
     (apply *))))

(defn solve-2
  ([] (solve-2 "resources/2020/day13.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     parse
     second
     vec
     h/indexed
     s2)))

;(solve-2 "resources/2020/day13.test.txt")
;(x-mod-n 7 0)
;(x-mod-n 17 13 2)


(deftest test-stuff [] 
  (test/are [x y] (= x y)
    754018 (s2 (h/indexed [67 7 59 61]))
    779210 (s2 (h/indexed [67 nil 7 59 61]))
    295 (solve-1 "resources/2020/day13.test.txt")
    1068781 (solve-2 "resources/2020/day13.test.txt")))
