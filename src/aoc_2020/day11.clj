(ns aoc-2020.day11
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defrecord State [seats taken nbmap])
(defrecord  Coord [x y])

(defn to-coord [{:keys [x y]}] (Coord. x y))

(defn initial-state [seats] (State. (set (map to-coord seats)) #{} #{}))

(defn make-nbmap-1 [{:keys [x y] :as seat} seats]
  [seat (keep seats [(Coord. (- x 1) (- y 1))
                     (Coord. (+ x 0) (- y 1))
                     (Coord. (+ x 1) (- y 1))
                     (Coord. (- x 1) (- y 0))
                     ;{:x (+ x 0) :y (- y 0))
                     (Coord. (+ x 1) (- y 0))
                     (Coord. (- x 1) (+ y 1))
                     (Coord. (+ x 0) (+ y 1))
                     (Coord. (+ x 1) (+ y 1))])])

(defn find-dir [c delta seats]
  (loop [at (merge-with + c delta) n 1]
    (cond
      (seats at) at
      (= n 100) nil
      :else (recur (merge-with + at delta) (inc n)))))

(defn make-nbmap-2 [seat seats]
  [seat (keep identity [(find-dir seat {:x -1, :y -1} seats)
                        (find-dir seat {:x  0, :y -1} seats)
                        (find-dir seat {:x +1, :y -1} seats)
                        (find-dir seat {:x -1, :y 0} seats)
                        ;(find-dir seat {:x 0, :y 0} seats)
                        (find-dir seat {:x +1, :y 0} seats)
                        (find-dir seat {:x -1, :y +1} seats)
                        (find-dir seat {:x  0, :y +1} seats)
                        (find-dir seat {:x +1, :y +1} seats)])])


(defn with-nbmap [func state]
  (assoc state :nbmap (into {} (map #(func % (:seats state)) (:seats state)))))


(defn n-neighbors [state c]
  (count (keep (:taken state) ((:nbmap state) c))))

(defn advance [state]
  (let [taken (:taken state)
        remaining (set/select #(< (n-neighbors state %) 4) taken)
        new-visitors (set/select #(= 0 (n-neighbors state %)) (:seats state))]
    (assoc state :taken (set/union remaining new-visitors))))

(defn advance-2 [state]
  (let [taken (:taken state)
        remaining (set/select #(< (n-neighbors state %) 5) taken)
        new-visitors (set/select #(= 0 (n-neighbors state %)) (:seats state))]
    (assoc state :taken (set/union remaining new-visitors))))


(defn solve-1
  ([] (solve-1 "resources/2020/day11.txt"))
  ([file]
   (let [seats (->>
                 (h/slurp-xy-map file)
                 first ; don't care for dimensions
                 keys
                 set)]
     (->> seats
       initial-state
       (with-nbmap make-nbmap-1)
       (iterate advance)
       (partition 2)
       (drop-while (fn [[a b]] (not= (:taken a) (:taken b))))
       ffirst
       :taken
       count))))

(defn solve-2
  ([] (solve-2 "resources/2020/day11.txt"))
  ([file]
   (let [seats (->> file
                 h/slurp-xy-map
                 first ; don't care for dimensions
                 keys
                 set)]
     (->> seats
       initial-state
       (with-nbmap make-nbmap-2)
       (iterate advance-2)
       (partition 2)
       (drop-while (fn [[a b]] (not= (:taken a) (:taken b))))
       ffirst
       :taken
       count))))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    37 (solve-1 "resources/2020/day11.test.txt")
    26 (solve-2 "resources/2020/day11.test.txt")))
