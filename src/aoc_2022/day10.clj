(ns aoc-2022.day10
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(def initial-state {:x 1})

(defn parse-command [c]
  (let [split (str/split c #" ")
        cmd (first split)]
    (cond
      (= cmd "noop") [0 identity]
      (= cmd "addx") [1 #(update % :x (partial + (Integer/parseInt (second split))))])))

(defn run-commands 
  ([state commands] (run-commands state (rest commands) 1 (first commands) []))
  ([state commands clock cmd accu]

   (cond
     (nil? cmd) accu
     (zero? (first cmd)) (recur ((second cmd) state) (rest commands) (inc clock) (first commands)                 (conj accu [clock state]))
     :else               (recur state                commands        (inc clock) [(dec (first cmd)) (second cmd)] (conj accu [clock state])))))


(defn interesting-state? 
  "problem #1"
  [[clock _state]]
  (zero? (mod (- clock 20) 40)))

(defn convert-to-pixel
  [[clock {:keys [:x]} ]]
  (let [pixel (mod (dec clock) 40)]
    (if (or (= pixel x) (= pixel (inc x)) (= pixel (dec x))) "#" ".")))


(defn signal-strength [[clock state]] (* clock (:x state)))

(defn solve-1
  ([] (solve-1 "resources/2022/day10.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (map parse-command)
     (run-commands initial-state)
     (filter interesting-state?)
     (map signal-strength)
     (reduce +))))


(defn output [pixel-lines]
  (println)
  (run! println pixel-lines))

(defn solve-2
  ([] (solve-2 "resources/2022/day10.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (map parse-command)
     (run-commands initial-state)
     (map convert-to-pixel)
     (partition 40)
     (map str/join)
     output)))

(def sample 
  [(parse-command "noop")
   (parse-command "addx 3")
   (parse-command "addx -5")])


(deftest test-stuff [] 
  (test/are [x y] (= x y)
    13140 (solve-1 "resources/2022/day10.test.txt")
    ; (solve-2 "resources/2022/day10.test.txt")
    ))
