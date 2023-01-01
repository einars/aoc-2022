(ns aoc-2020.day12
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn parse [s]
  (let [[_ cmd n] (first (re-seq #"([NWSELRF])(\d+)" s))]
    [(keyword cmd) (Integer/parseInt n)]))

(def initial-state 
  {:facing :E
   :x 0
   :y 0})

(def left-turns {:N :W, :W :S, :S :E, :E :N})
(def right-turns {:N :E, :E :S, :S :W, :W :N})

(defn turn-left [facing amount]
  (if (<= amount 0) 
    facing
    (turn-left (left-turns facing) (- amount 90))))

(defn turn-right [facing amount]
  (if (<= amount 0) 
    facing
    (turn-right (right-turns facing) (- amount 90))))

(defn advance [state [cmd n]]
  (condp = cmd
    :N (update state :y #(+ % n))
    :S (update state :y #(- % n))
    :W (update state :x #(+ % n))
    :E (update state :x #(- % n))
    :F (advance state [(:facing state) n])
    :R (update state :facing #(turn-right % n))
    :L (update state :facing #(turn-left % n))))

(defn get-answer [{:keys [x y]}]
  (+ (abs x) (abs y)))

(defn solve-1
  ([] (solve-1 "resources/2020/day12.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (map parse)
     (reduce advance initial-state)
     get-answer
     ; ...
     )))

(solve-1 "resources/2020/day12.test.txt")

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    25 (solve-1 "resources/2020/day12.test.txt")))
