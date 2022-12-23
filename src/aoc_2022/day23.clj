(ns aoc-2022.day23
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn score [{:keys [board]}]
  (let [x-min (apply min (map :x board))
        x-max (apply max (map :x board))
        y-min (apply min (map :y board))
        y-max (apply max (map :y board))]
    (- (* (inc (- y-max y-min)) (inc (- x-max x-min)))
      (count board))))

(defn get-n [elf] (update elf :y dec))
(defn get-s [elf] (update elf :y inc))
(defn get-w [elf] (update elf :x dec))
(defn get-e [elf] (update elf :x inc))

(defn get-ne [{:keys [x y]}] {:x (inc x) :y (dec y)})
(defn get-nw [{:keys [x y]}] {:x (dec x) :y (dec y)})
(defn get-se [{:keys [x y]}] {:x (inc x) :y (inc y)})
(defn get-sw [{:keys [x y]}] {:x (dec x) :y (inc y)})

(defn test-0 [elf board]
  (cond
    (nil? (or (board (get-nw elf)) (board (get-n elf)) (board (get-ne elf)))) (get-n elf)
    (nil? (or (board (get-sw elf)) (board (get-s elf)) (board (get-se elf)))) (get-s elf)
    (nil? (or (board (get-sw elf)) (board (get-w elf)) (board (get-nw elf)))) (get-w elf)
    (nil? (or (board (get-ne elf)) (board (get-e elf)) (board (get-se elf)))) (get-e elf)
    :else elf))

(defn test-1 [elf board]
  (cond
    (nil? (or (board (get-sw elf)) (board (get-s elf)) (board (get-se elf)))) (get-s elf)
    (nil? (or (board (get-sw elf)) (board (get-w elf)) (board (get-nw elf)))) (get-w elf)
    (nil? (or (board (get-ne elf)) (board (get-e elf)) (board (get-se elf)))) (get-e elf)
    (nil? (or (board (get-nw elf)) (board (get-n elf)) (board (get-ne elf)))) (get-n elf)
    :else elf))

(defn test-2 [elf board]
  (cond
    (nil? (or (board (get-sw elf)) (board (get-w elf)) (board (get-nw elf)))) (get-w elf)
    (nil? (or (board (get-ne elf)) (board (get-e elf)) (board (get-se elf)))) (get-e elf)
    (nil? (or (board (get-nw elf)) (board (get-n elf)) (board (get-ne elf)))) (get-n elf)
    (nil? (or (board (get-sw elf)) (board (get-s elf)) (board (get-se elf)))) (get-s elf)
    :else elf))

(defn test-3 [elf board]
  (cond
    (nil? (or (board (get-ne elf)) (board (get-e elf)) (board (get-se elf)))) (get-e elf)
    (nil? (or (board (get-nw elf)) (board (get-n elf)) (board (get-ne elf)))) (get-n elf)
    (nil? (or (board (get-sw elf)) (board (get-s elf)) (board (get-se elf)))) (get-s elf)
    (nil? (or (board (get-sw elf)) (board (get-w elf)) (board (get-nw elf)))) (get-w elf)
    :else elf))

(defn somebody-around? [elf board]
  (or (board (get-nw elf))
    (board (get-n elf))
    (board (get-ne elf))
    (board (get-e elf))
    (board (get-se elf))
    (board (get-s elf))
    (board (get-sw elf))
    (board (get-w elf))))

(defn planned-move [elf board iteration]
  (if (somebody-around? elf board)
    (condp = (mod iteration 4)
      0 (test-0 elf board)
      1 (test-1 elf board)
      2 (test-2 elf board)
      3 (test-3 elf board))
    elf))

(defn step
  [{:keys [board n]}]
  (let [planned-moves (reduce (fn [plan elf] (update plan (planned-move elf board n) #(if % (conj % elf) [elf])))
                        {} board)]
    {:board (set (mapcat (fn [[pos elves]] (if (> (count elves) 1) elves [pos])) planned-moves))
     :n (inc n)}))
    

(defn make-board [elves] 
  {:board (set elves)
   :n 0})

(defn solve-1
  ([] (solve-1 "resources/2022/day23.txt"))
  ([file]
   (->>
     (h/slurp-xy-map file)
     (first)
     (keys)
     (make-board)
     (iterate step)
     (drop 10)
     (first)
     (score))))

(defn solve-2
  ([] (solve-2 "resources/2022/day23.txt"))
  ([file]
   (let [board-seq (->>
                     (h/slurp-xy-map file)
                     (first)
                     (keys)
                     (make-board)
                     (iterate step))
         ]
     (reduce (fn [prev cur] 
               (if (= (:board prev) (:board cur)) 
                 (reduced (:n cur)) 
                 cur)) 
       {} board-seq))))

;(solve-1 "resources/2022/day23.test.txt")
;(solve-2 "resources/2022/day23.txt")
(deftest test-stuff [] 
  (test/are [x y] (= x y)
    110 (solve-1 "resources/2022/day23.test.txt")
    20 (solve-2 "resources/2022/day23.test.txt")))
