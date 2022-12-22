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

(defn current-cube 
  [{:keys [csize at]}]
  [(+ (quot (at :x) csize) (* (quot (at :y) csize) 4))
   (mod (at :x) csize)
   (mod (at :y) csize)
   (- csize 1 (mod (at :x) csize))
   (- csize 1 (mod (at :y) csize))])

(defn at-cube 
  [csize n {:keys [x y]}]
  (condp = n
    0 {:x (+ x) :y (+ y)}
    1 {:x (+ csize x) :y (+ y)}
    2 {:x (+ csize csize x) :y (+ y)}
    3 {:x (+ csize csize csize x) :y (+ y)}
    4 {:x (+ x) :y (+ csize y)}
    5 {:x (+ csize x) :y (+ csize y)}
    6 {:x (+ csize csize x) :y (+ csize y)}
    7 {:x (+ csize csize csize x) :y (+ csize y)}

    8 {:x (+ x) :y (+ csize csize y)}
    9 {:x (+ csize x) :y (+ csize csize y)}
    10 {:x (+ csize csize x) :y (+ csize csize y)}
    11 {:x (+ csize csize csize x) :y (+ csize csize y)}

    12 {:x (+ x) :y (+ csize csize csize y)}
    ))

(defn cube-wrap-n
  [_drawing {:keys [csize] :as state}]
  (let [[cube rx ry invx invy] (current-cube state)]
    (condp = cube
      2 (-> state
          (assoc :facing :S)
          (assoc :at (at-cube csize 2 {:x invx, :y 0})))
      4 (-> state
          (assoc :facing :S)
          (assoc :at (at-cube csize 4 {:x invx :y 0})))
      5 (-> state
          (assoc :facing :E)
          (assoc :at (at-cube csize 2 {:x 0 :y rx})))
      11 (-> state
           (assoc :facing :W)
           (assoc :at (at-cube csize 6 {:x invx :y 0}))))))

(defn cube-wrap-s
  [_drawing {:keys [csize] :as state}]
  (let [[cube rx ry invx invy] (current-cube state)]
    (condp = cube
      4 (-> state
          (assoc :facing :N)
          (assoc :at (at-cube csize 10 {:x invx, :y ry})))
      5 (-> state
          (assoc :facing :E)
          (assoc :at (at-cube csize 10 {:x 0 :y invx})))
      10 (-> state
           (assoc :facing :N)
           (assoc :at (at-cube csize 4 {:x invx :y ry})))
      11 (-> state
           (assoc :facing :E)
           (assoc :at (at-cube csize 6 {:x 0 :y invx}))))))

(defn cube-wrap-w
  [_drawing {:keys [csize] :as state}]
  (let [[cube rx ry invx invy] (current-cube state)]
    (condp = cube
      0 (trace [state cube rx ry])
      2 (-> state
          (assoc :facing :N)
          (assoc :at (at-cube csize 5 {:x ry, :y 0})))
      4 (-> state
          (assoc :facing :N)
          (assoc :at (at-cube csize 11 {:x invy :y invx})))
      10 (-> state
           (assoc :facing :N)
           (assoc :at (at-cube csize 5 {:x invy :y invx}))))))

(defn cube-wrap-e
  [_drawing {:keys [csize] :as state}]
  (let [[cube rx ry invx invy] (current-cube state)]
    (condp = cube
      2 (-> state
          (assoc :facing :W)
          (assoc :at (at-cube csize 11 {:x invx, :y ry})))
      6 (-> state
          (assoc :facing :S)
          (assoc :at (at-cube csize 11 {:x invy :y 0})))
      11 (-> state
           (assoc :facing :W)
           (assoc :at (at-cube csize 2 {:x invx :y ry}))))))


(defn pube-wrap
  [_drawing {:keys [csize] :as state}]
  (let [[cube rx ry invx invy] (current-cube state)
        bottom (dec csize)
        right (dec csize)]
    ;(trace [state cube rx ry])
    (condp = [cube (:facing state)]
      [1 :N] (-> state
               (assoc :facing :E)
               (assoc :at (at-cube csize 12 {:x 0, :y rx})))
      [1 :W] (-> state
               (assoc :facing :E)
               (assoc :at (at-cube csize 8 {:x 0, :y invy})))
      [2 :N] (-> state
               (assoc :facing :N)
               (assoc :at (at-cube csize 12 {:x rx :y bottom})))
      [2 :E] (-> state
               (assoc :facing :W)
               (assoc :at (at-cube csize 9 {:x right :y invy})))
      [2 :S] (-> state
               (assoc :facing :W)
               (assoc :at (at-cube csize 5 {:x right :y rx})))
      [5 :W] (-> state
               (assoc :facing :S)
               (assoc :at (at-cube csize 8 {:x ry :y 0})))
      [5 :E] (-> state
               (assoc :facing :N)
               (assoc :at (at-cube csize 2 {:x ry :y bottom})))
      [8 :N] (-> state
               (assoc :facing :E)
               (assoc :at (at-cube csize 5 {:x 0 :y rx})))
      [8 :W] (-> state
               (assoc :facing :E)
               (assoc :at (at-cube csize 1 {:x 0 :y invy})))
      [9 :E] (-> state
               (assoc :facing :W)
               (assoc :at (at-cube csize 2 {:x right :y invy})))
      [9 :S] (-> state
               (assoc :facing :W)
               (assoc :at (at-cube csize 12 {:x right :y rx})))
      [12 :W] (-> state
                (assoc :facing :S)
                (assoc :at (at-cube csize 1 {:x ry :y 0})))
      [12 :S] (-> state
                (assoc :facing :S)
                (assoc :at (at-cube csize 2 {:x rx :y 0})))
      [12 :E] (-> state
                (assoc :facing :N)
                (assoc :at (at-cube csize 9 {:x ry :y bottom}))))))


(defn tile-facing [drawing state test-at wrap-fn]
  (if-let [tile (drawing test-at)]
    [tile (assoc state :at test-at)]
    (let [new-state (wrap-fn drawing state)]
      [(drawing (:at new-state)) new-state])))


(def pt1-wrappers {:N wrap-n :S wrap-s :W wrap-w :E wrap-e})
(def pt2-wrappers-test {:N cube-wrap-n :S cube-wrap-s :W cube-wrap-w :E cube-wrap-e})
(def pt2-wrappers-prod {:N pube-wrap :S pube-wrap :W pube-wrap :E pube-wrap})

(def ^:dynamic *wrappers* pt1-wrappers)

(defn take-step [drawing {:keys [facing at] :as state}]
  ;(trace [:step state])
  (let [{:keys [x y]} at
        [tile new-state] (condp = facing
                           :N (tile-facing drawing state {:x x :y (dec y)} (*wrappers* :N))
                           :S (tile-facing drawing state {:x x :y (inc y)} (*wrappers* :S))
                           :W (tile-facing drawing state {:x (dec x) :y y} (*wrappers* :W))
                           :E (tile-facing drawing state {:x (inc x) :y y} (*wrappers* :E)))]
    ;(trace [facing at new-state])
    (when (= tile \.) new-state)))

(defn exec [[drawing state] command]
  ;(trace "exec" [state command])
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

(defn initial-state [drawing max-x]
  {:facing :E
   :csize (if (< max-x 20) 4 50)
   :at {:x (->> drawing
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
         [drawing _] (binding [h/*map-ignore* #{\space}] (h/make-xy-map (str/split drawing #"\n")))
         max-x (first (sort > (map :x (keys drawing))))
         path (parse-path path)
         state (initial-state drawing max-x)]
     (make-score (second (reduce exec [drawing state] path))))))

(defn solve-2
  [& param]
  (binding [*wrappers* pt2-wrappers-prod] (apply solve-1 param)))

;(solve-1 "resources/2022/day22.test.txt")
;(solve-2 "resources/2022/day22.test.txt")
;(binding [*wrappers* pt2-wrappers-prod] (solve-2 "resources/2022/day22.txt"))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    [10 :R 5 :L 5 :R 10 :L 4 :R 5 :L 5] (parse-path "10R5L5R10L4R5L5")
    6032 (solve-1 "resources/2022/day22.test.txt")
    5031 (binding [*wrappers* pt2-wrappers-test] (solve-1 "resources/2022/day22.test.txt"))))
