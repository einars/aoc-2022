(ns aoc-2022.day17
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))


(def figures
  (cycle 
    [[[0 0] [1 0] [2 0] [3 0]]
     [[1 0] [0 1] [1 1] [2 1] [1 2]]
     [[0 0] [1 0] [2 0] [2 1] [2 2]]
     [[0 0] [0 1] [0 2] [0 3]]
     [[0 0] [0 1] [1 0] [1 1]]]))

(def initial-board #{})

(defn move-fig [[x y] move]
  (condp = move
    \> [(inc x) y]
    \< [(dec x) y]))

(defn may-place [board fig [x y]]

  (not (some (fn [[fx fy]]
               (let [tx (+ x fx)
                     ty (+ y fy)]
                 (or (< tx 0)
                   (> tx 6)
                   (< ty 0)
                   (board [tx ty])))) fig)))

(defn materialize [board figure [x y]]
  (reduce conj board (map (fn [[fx fy]] [(+ x fx) (+ y fy)]) figure)))

(defn calc-height [board] (or (first (sort > (map (comp inc second) board))) 0))

(defn place-fig [[board [figure & rest-figs] moveset]]

  (let [fig-x 2
        fig-y (+ 3 (calc-height board))]
    (loop [[move & rest-moves] moveset 
           fig-at [fig-x fig-y]]

      (let [side-fig-at (move-fig fig-at move)
            side-fig-at (if (may-place board figure side-fig-at) side-fig-at fig-at) ; horizontal movement if possible
            dn-fig-at [(first side-fig-at) (dec (second side-fig-at))] ; try moving down
            ]
        (if (may-place board figure dn-fig-at)
          (recur rest-moves dn-fig-at)
          [(materialize board figure side-fig-at) rest-figs rest-moves] ; finish, no downward movement
          )))))


(defn solve-1
  ([] (solve-1 "resources/2022/day17.txt"))
  ([file]
   (let [moveset (cycle (str/trim (slurp file)))]
     (calc-height (first (nth (iterate place-fig [initial-board figures moveset]) 2022))))))

;(solve-1 "resources/2022/day17.test.txt")

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    3068 (solve-1 "resources/2022/day17.test.txt")))
