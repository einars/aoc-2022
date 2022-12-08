(ns aoc-2022.day8
  (:require [clojure.test :as test :refer [deftest]])
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pprint])
  (:require [clojure.tools.trace :refer [deftrace, trace]])
  (:require [aoc.helpers :as h]))

(defn walk-trees-visible
  [forest coord delta]
  (loop [coord coord, height (int (forest coord)), accum #{coord}]
    (let [new-coord {:x (+ (:x coord) (get delta :dx 0)),
                     :y (+ (:y coord) (get delta :dy 0))}]
      (if-let [new-height (forest new-coord)]
        (let [new-height (int new-height)]
          (if (> new-height height)
            (recur new-coord new-height (conj accum new-coord))
            (recur new-coord height accum)))
        accum) ) ) )

(defn trees-visible-dir
  "walk in direction delta from coordinates given by root-pred"
  [forest root-pred delta]
  (let [starting-coords (filter root-pred (keys forest))]
    (reduce set/union #{} (for [c starting-coords] (walk-trees-visible forest c delta)))))

(defn tree-score-dir
  [forest coord delta]
  (let [height (int (forest coord))]
    (loop [coord coord, score 0]
      (let [new-coord {:x (+ (:x coord) (get delta :dx 0)),
                       :y (+ (:y coord) (get delta :dy 0))}]
        (if-let [new-height (forest new-coord)]
          (if (>= (int new-height) height)
            (inc score)
            (recur new-coord (inc score)))
          score)))))

(defn visible-trees
  [[forest dimensions]]
  (reduce set/union #{}
    [(trees-visible-dir forest #(= (:y %) 0) {:dy +1})
     (trees-visible-dir forest #(= (:y %) (dec (dimensions :y))) {:dy -1})
     (trees-visible-dir forest #(= (:x %) 0) {:dx 1})
     (trees-visible-dir forest #(= (:x %) (dec (dimensions :x))) {:dx -1})
     ]))

(defn tree-score-at
  [forest coord]
  (reduce *
    [(tree-score-dir forest coord {:dy +1})
     (tree-score-dir forest coord {:dy -1})
     (tree-score-dir forest coord {:dx 1})
     (tree-score-dir forest coord {:dx -1})]))

(defn find-best-tree-score
  [[forest _dimensions]]
  ;(sort-by second (map #(vec [% (tree-score-at forest %)]) (keys forest))))
  (first (sort > (map #(tree-score-at forest %) (keys forest)))))


(defn solve-1
  ([] (solve-1 "resources/2022/day8.txt"))
  ([file]
   (count (visible-trees (h/slurp-xy-map file)))))

(defn solve-2
  ([] (solve-2 "resources/2022/day8.txt"))
  ([file]
   (find-best-tree-score (h/slurp-xy-map file))))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    21 (solve-1 "resources/2022/day8.test.txt")
    8 (solve-2 "resources/2022/day8.test.txt")
    4 (tree-score-at (first (h/slurp-xy-map "resources/2022/day8.test.txt")) {:x 2 :y 1})
    8 (tree-score-at (first (h/slurp-xy-map "resources/2022/day8.test.txt")) {:x 2 :y 3})
    ))
