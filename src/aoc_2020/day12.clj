(ns aoc-2020.day12
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn parse [s]
  (let [[_ cmd n] (first (re-seq #"([NWSELRF])(\d+)" s))]
    [(keyword cmd) (Integer/parseInt n)]))

(defn parse-2 [s]
  (let [[_ cmd n] (first (re-seq #"([NWSELRF])(\d+)" s))]
    [(keyword (str cmd "2")) (Integer/parseInt n)]))

(def initial-state 
  {:facing :E
   :x 0
   :y 0
   :wpx 10
   :wpy 1})

(def left-turns {:N :W, :W :S, :S :E, :E :N})
(def right-turns {:N :E, :E :S, :S :W, :W :N})

(defn turn-left [facing amount]
  (if (<= amount 0) 
    facing
    (turn-left (left-turns facing) (- amount 90))))

(defn turn-right [facing amount]
  (if (<= amount 0) 
    facing
    (turn-right (right-turns facing) (- amount 90))))

(defn turn-wp-left [{:keys [wpx wpy] :as state} amount]
  (if (<= amount 0) 
    state
    (turn-wp-left 
      (-> state
        (assoc :wpx (- wpy))
        (assoc :wpy wpx)) 
      (- amount 90))))

(defn turn-wp-right [{:keys [wpx wpy] :as state} amount]
  (if (<= amount 0) 
    state
    (turn-wp-right
      (-> state
        (assoc :wpx wpy)
        (assoc :wpy (- wpx)))
      (- amount 90))))

(defn advance [state [cmd n]]
  (condp = cmd
    :N (update state :y #(+ % n))
    :S (update state :y #(- % n))
    :W (update state :x #(- % n))
    :E (update state :x #(+ % n))
    :F (advance state [(:facing state) n])
    :R (update state :facing #(turn-right % n))
    :L (update state :facing #(turn-left % n))

    :N2 (update state :wpy #(+ % n))
    :S2 (update state :wpy #(- % n))
    :W2 (update state :wpx #(- % n))
    :E2 (update state :wpx #(+ % n))
    :F2 (-> state 
          (update :x #(+ % (* n (:wpx state))))
          (update :y #(+ % (* n (:wpy state)))))
    :R2 (turn-wp-right state n)
    :L2 (turn-wp-left state n)))

(defn get-answer [{:keys [x y]}]
  (+ (abs x) (abs y)))

(defn solve-1
  ([] (solve-1 "resources/2020/day12.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (map parse)
     (reduce advance initial-state)
     get-answer)))

(defn solve-2
  ([] (solve-2 "resources/2020/day12.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (map parse-2)
     (reduce advance initial-state)
     get-answer)))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    25 (solve-1 "resources/2020/day12.test.txt")
    286 (solve-2 "resources/2020/day12.test.txt")))
