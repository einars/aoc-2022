(ns aoc-2022.day2
  (:require [clojure.test :as test :refer [deftest]])
  (:require [clojure.string :as str])
  (:require [aoc-2022.helpers :as h]))

(def shape-score {:rock 1, :paper 2, :scissors 3} )

(defn beats? 
  [shape over]
  (or 
    (and (= shape :rock) (= over :scissors))
    (and (= shape :paper) (= over :rock))
    (and (= shape :scissors) (= over :paper))))

(defn play-rock-scissors-round
  "Plays a round of rock n scissors, returns scores for both players
  A|X = rock
  B|Y = paper
  C|Z = scissors
  "
  [a b]
  (let [mapping {:a :rock, :x :rock, :b :paper, :y :paper, :c :scissors, :z :scissors}
        ma (mapping (keyword (str/lower-case a)))
        mb (mapping (keyword (str/lower-case b)))]

      (cond
        (= ma mb) [(+ 3 (shape-score ma)) (+ 3 (shape-score mb))]
        (beats? ma mb) [(+ 6 (shape-score ma)) (shape-score mb)]
        :else [(shape-score ma) (+ 6 (shape-score mb))])))

(defn follow-guide
  [lines]
  (reduce 
    (fn [[a b] [aa bb]] [(+ a aa) (+ b bb)] )
    [0 0]
    (mapv #(apply play-rock-scissors-round (str/split % #" ")) lines)))




(deftest test-rock-scissors []
  (test/are [x y] (= x y)
    8 (second (play-rock-scissors-round "A" "Y"))
    1 (second (play-rock-scissors-round "B" "X"))
    6 (second (play-rock-scissors-round "C" "Z"))))

(deftest test-1 []
  (test/is (= 15 (second (follow-guide (h/slurp-strings "resources/day2.test.txt"))))))

(test/run-tests)

(defn solve-1 []
  (second (follow-guide (h/slurp-strings "resources/day2.txt"))))

(defn solve-2 []
  )


