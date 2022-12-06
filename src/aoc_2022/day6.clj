(ns aoc-2022.day6
  (:require [clojure.test :as test :refer [deftest]])
  ;(:require [clojure.tools.trace :as trace])
  (:require [aoc.helpers :as h]))

(defn transmission-header-idx
  [transmission len]
  (let [blocks (partition len 1 transmission)]
    (first (keep-indexed (fn [idx bytes] (when (apply distinct? bytes) idx)) blocks))))

(defn transmission-start-idx
  [transmission]
  (+ 4 (transmission-header-idx transmission 4)))

(defn message-start-idx
  [transmission]
  (+ 14 (transmission-header-idx transmission 14)))

(defn solve-1
  ([] (solve-1 "resources/2022/day6.txt"))
  ([file]
    (->>
      (h/slurp-strings file)
      first
      transmission-start-idx)))

(defn solve-2
  ([] (solve-2 "resources/2022/day6.txt"))
  ([file]
    (->>
      (h/slurp-strings file)
      first
      message-start-idx)))


(deftest test-stuff [] 
  (test/are [x y] (= x y)
    5 (transmission-start-idx (seq "bvwbjplbgvbhsrlpgdmjqwftvncz"))
    6 (transmission-start-idx (seq "nppdvjthqldpwncqszvftbrmjlhg"))
    10 (transmission-start-idx (seq "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"))
    19 (message-start-idx (seq "mjqjpqmgbljsphdztnvjfqwrcgsmlb"))
    ))

