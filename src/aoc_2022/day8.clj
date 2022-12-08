(ns aoc-2022.day8
  (:require [clojure.test :as test :refer [deftest]])
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pprint])
  (:require [clojure.tools.trace :refer [deftrace, trace]])
  (:require [aoc.helpers :as h]))

(defn walk-trees-visible
  [xy coord delta]
  (loop [coord coord, height (int (xy coord)), accum #{coord}]
    (let [new-coord {:x (+ (:x coord) (get delta :dx 0)),
                     :y (+ (:y coord) (get delta :dy 0))}]
      (if-let [new-height (xy new-coord)]
        (let [new-height (int new-height)]
          (if (> new-height height)
            (recur new-coord new-height (conj accum new-coord))
            (recur new-coord height accum)))
        accum) ) ) )

(defn trees-visible-dir
  "walk in direction delta from coordinates given by root-pred"
  [xy root-pred delta]
  (let [starting-coords (filter root-pred (keys xy))]
    (reduce set/union #{} (for [c starting-coords] (walk-trees-visible xy c delta)))))

(defn visible-trees
  [[xy dimensions]]
  (reduce set/union #{}
    [(trees-visible-dir xy #(= (:y %) 0) {:dy +1})
     (trees-visible-dir xy #(= (:y %) (dec (dimensions :y))) {:dy -1})
     (trees-visible-dir xy #(= (:x %) 0) {:dx 1})
     (trees-visible-dir xy #(= (:x %) (dec (dimensions :x))) {:dx -1})
     ]))

(defn solve-1
  ([] (solve-1 "resources/2022/day8.txt"))
  ([file]
   (count (visible-trees (h/slurp-xy-map file)))))


(deftest test-stuff [] 
  (test/are [x y] (= x y)
    21 (solve-1 "resources/2022/day8.test.txt")
    ; 0 (solve-2 "resources/2022/day8.test.txt")
    ))
