(ns aoc-2022.day22
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(def turn {
           :E {:R :S, :L :N}
           :S {:R :W, :L :E}
           :W {:R :N, :L :S}
           :N {:R :E, :L :W}})


(defn wrap-n
  [drawing state]
  (let [x (get-in state [:at :x])]
    (assoc-in state [:at :y] (->> (keys drawing)
                               (filter #(= (:x %) x))
                               (map :y)
                               (apply max)))))

(defn wrap-s
  [drawing state]
  (let [x (get-in state [:at :x])]
    (assoc-in state [:at :y] (->> (keys drawing)
                               (filter #(= (:x %) x))
                               (map :y)
                               (apply min)))))

(defn wrap-e
  [drawing state]
  (let [y (get-in state [:at :y])]
    (assoc-in state [:at :x] (->> (keys drawing)
                               (filter #(= (:y %) y))
                               (map :x)
                               (apply min)))))
(defn wrap-w
  [drawing state]
  (let [y (get-in state [:at :y])]
    (assoc-in state [:at :x] (->> (keys drawing)
                               (filter #(= (:y %) y))
                               (map :x)
                               (apply max)))))

(defn tile-facing [drawing state wrap-fn]
  (if-let [tile (drawing (:at state))]
    [tile state]
    (let [new-state (wrap-fn drawing state)]
      [(drawing (:at new-state)) new-state])))

(defn take-step [drawing {:keys [facing at] :as state}]
  ;(trace [:step state])
  (let [{:keys [x y]} at
        [tile new-state] (condp = facing
                           :N (tile-facing drawing (assoc state :at {:x x :y (dec y)}) wrap-n)
                           :S (tile-facing drawing (assoc state :at {:x x :y (inc y)}) wrap-s)
                           :W (tile-facing drawing (assoc state :at {:x (dec x) :y y}) wrap-w)
                           :E (tile-facing drawing (assoc state :at {:x (inc x) :y y}) wrap-e))]
    (when (= tile \.) new-state)))

(defn exec [[drawing state] command]
  ; (trace "exec" [state command])
  (condp = command
    :L [drawing (update state :facing #((turn %) :L))]
    :R [drawing (update state :facing #((turn %) :R))]
    0 [drawing state]
    (if-let [new-state (take-step drawing state)]
      (recur [drawing new-state] (dec command))
      [drawing state])))

(defn parse-path
  [s]
  (map (fn [c] (condp = c
                 "R" :R
                 "L" :L
                 (Integer/parseInt c)
                 )) (re-seq #"[\d]+|L|R" s)))

(defn initial-state [drawing]
  {:facing :E :at {:x (->> drawing
                        (keys)
                        (filter #(zero? (:y %)))
                        (map :x)
                        (apply min)), :y 0}})

(defn make-score [state]
  (let [row (inc (get-in state [:at :y]))
        col (inc (get-in state [:at :x]))
        facing ({:E 0, :S 1, :W 2, :N 3} (state :facing))]

    (+ (* row 1000) (* 4 col) facing)))

(defn solve-1
  ([] (solve-1 "resources/2022/day22.txt"))
  ([file]
   (let [[drawing path] (str/split (slurp file) #"\n\n")
         [drawing dimensions] (binding [h/*map-ignore* #{\space}] (h/make-xy-map (str/split drawing #"\n")))
         path (parse-path path)
         state (initial-state drawing)]
     (make-score (second (reduce exec [drawing state] path))))))

(solve-1 "resources/2022/day22.test.txt")

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    [10 :R 5 :L 5 :R 10 :L 4 :R 5 :L 5] (parse-path "10R5L5R10L4R5L5")
    6032 (solve-1 "resources/2022/day22.test.txt")))
