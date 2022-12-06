(ns aoc-2016.day1
  (:require [clojure.test :as test :refer [deftest]])
  (:require [clojure.string :as str])
  (:require [clojure.tools.trace :as trace])
  (:require [aoc.helpers :as h]))

(def rt [1 0])
(def lt [-1 0])
(def up [0 1])
(def dn [0 -1])

(def turn-left {up lt, lt dn, dn rt, rt up })
(def turn-right {up rt, rt dn, dn lt, lt up })

(def turn {\L turn-left, \R turn-right})

(defn parse-move
  [move]
  {:dir (first move)
   :amount (Integer/parseInt (str/join (rest move)))})

(defn apply-move 
  [{:keys [dir amount]} pos facing]
  (let [facing-now ((turn dir) facing)]
  [(map + pos (map (partial * amount) facing-now))
   facing-now]))

(defn follow 
  [trail] 
  (loop [trail trail, pos [0 0], facing up]
    (if (seq trail)
      (let [[new-pos new-facing] (apply-move (first trail) pos facing)]
        (recur (rest trail) new-pos new-facing))
      pos)))

(defn solve-1
  ([] (solve-1 "resources/2016/day1.txt"))
  ([file]
    (->>
      (str/split (str/trim (slurp file)) #", ")
      (map parse-move)
      follow
      (map abs)
      (reduce +))))

(comment defn solve-2
  ([] (solve-2 "resources/2016/day1.txt"))
  ([file]
    (->>
      (str/split (slurp file) #", ")
      ; ...
      )))

(def r2 {:dir \R, :amount 2})

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    {:dir \L :amount 21} (parse-move "L21")
    [0 -2] (follow [r2 r2 r2])
    [0 0] (follow [r2 r2 r2 r2])
    [10 2] (follow [{:dir \R, :amount 5} {:dir \L :amount 5} {:dir \R :amount 5} {:dir \R :amount 3}])
    ))
