(ns aoc-2022.day2
  (:require [clojure.test :as test :refer [deftest]])
  (:require [clojure.string :as str])
  (:require [clojure.tools.trace :as trace :refer [deftrace]])
  (:require [aoc.helpers :as h]))

(def shape-score {:rock 1, :paper 2, :scissors 3})

(defn part-1-mapping [p1 p2]
  (let [m {:A :rock, :X :rock, :B :paper, :Y :paper, :C :scissors, :Z :scissors}]
    (list (m p1) (m p2))))

(defn part-2-mapping [p1 p2]
  (let [m {:A :rock
          ,:B :paper
          ,:C :scissors
          ,:X {:rock :scissors, :paper :rock,     :scissors :paper} ; lose
          ,:Y {:rock :rock,     :paper :paper,    :scissors :scissors} ; draw
          ,:Z {:rock :paper,    :paper :scissors, :scissors :rock}}] ; win
      (list (m p1) ((m p2) (m p1)))))

(defn beats? 
  [shape over]
  (or 
    (and (= shape :rock) (= over :scissors))
    (and (= shape :paper) (= over :rock))
    (and (= shape :scissors) (= over :paper))))

(defn play-rock-scissors-round
  "Plays a round of rock n scissors, returns scores for both players"
  [mapping a b]
  (let [[ma mb] (mapping a b)]
      (cond
        (= ma mb)      [(+ 3 (shape-score ma)) (+ 3 (shape-score mb))]
        (beats? ma mb) [(+ 6 (shape-score ma)) (shape-score mb)]
        :else          [(shape-score ma)       (+ 6 (shape-score mb))])))

(def play-rock-scissors-1 (partial play-rock-scissors-round part-1-mapping))
(def play-rock-scissors-2 (partial play-rock-scissors-round part-2-mapping))

(defn follow-guide
  [lines rock-scissors-fn]
  (reduce 
    (fn [[a b] [aa bb]] [(+ a aa) (+ b bb)] )
    [0 0]
    (mapv #(apply rock-scissors-fn (map keyword (str/split % #" "))) lines)))


(deftest test-rock-scissors []
  (test/are [x y] (= x y)
    8 (second (play-rock-scissors-1 :A :Y))
    1 (second (play-rock-scissors-1 :B :X))
    6 (second (play-rock-scissors-1 :C :Z))
    4 (second (play-rock-scissors-2 :A :Y))
    1 (second (play-rock-scissors-2 :B :X))
    7 (second (play-rock-scissors-2 :C :Z))))

(deftest test-1 []
  (test/is (= 15 (second (follow-guide (h/slurp-strings "resources/day2.test.txt") play-rock-scissors-1)))))

(deftest test-2 []
  (test/is (= 12 (second (follow-guide (h/slurp-strings "resources/day2.test.txt") play-rock-scissors-2)))))

;(test/run-tests)

(defn solve-1 []
  (second (follow-guide (h/slurp-strings "resources/day2.txt") play-rock-scissors-1)))

(defn solve-2 []
  (second (follow-guide (h/slurp-strings "resources/day2.txt") play-rock-scissors-2)))


