(ns aoc-2024.day3
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [instaparse.core :as insta]
   [aoc.helpers :as h]))

(def sample
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(def sample-2
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(def parser (insta/parser "
  <input> = (mul | do | dont | <crap>)*
  crap = !(mul | do | dont) (#'.' | '\\r' | '\\n')
  mul = <'mul('> int <','> int <')'>
  do = <'do()'>
  dont = <'don\\'t()'>
  int = #'\\d+'
  "))


(defn parse-input
  [s]
  (mapv (fn [[x a b]]
          (if
            (= :mul x) [:mul (parse-long (second a)) (parse-long (second b))]
            [x]))
    (parser s)))

(defn pt1
  [task]
  (reduce + (mapv (fn [[cmd a b]] (if (#{:mul} cmd) (* a b) 0)) task)))

(defn pt2
  ([xs]
   (pt2 xs 0 true))
  ([[[cmd x y] & rest] accu do?]
   (if (nil? cmd)
     accu

     (condp = cmd
       :do (recur rest accu true)
       :dont (recur rest accu false)
       :mul (recur rest
              (if do? (+ accu (* x y)) accu)
              do?)))))

(defn solve-1
  ([] (solve-1 (slurp "resources/2024/day3.txt")))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp "resources/2024/day3.txt")))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    161 (pt1 (parse-input sample))
    48 (pt2 (parse-input sample-2))))



(comment
  (solve-1)
  (solve-2))
