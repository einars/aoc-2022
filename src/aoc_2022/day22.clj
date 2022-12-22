(ns aoc-2022.day22
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(def turn {:E {:R :S, :L :N}
           :S {:R :W, :L :E}
           :W {:R :N, :L :S}
           :N {:R :E, :L :W}})


(defn wrap-pt1
  [drawing state]
  (let [x (get-in state [:at :x])
        y (get-in state [:at :y])]
    (condp = (:facing state)
      :N (assoc-in state [:at :y] (->> (keys drawing)
                                    (filter #(= (:x %) x))
                                    (map :y)
                                    (apply max)))
      :S (assoc-in state [:at :y] (->> (keys drawing)
                                    (filter #(= (:x %) x))
                                    (map :y)
                                    (apply min)))
      :E (assoc-in state [:at :x] (->> (keys drawing)
                                    (filter #(= (:y %) y))
                                    (map :x)
                                    (apply min)))
      :W (assoc-in state [:at :x] (->> (keys drawing)
                                    (filter #(= (:y %) y))
                                    (map :x)
                                    (apply max))))))

(defn current-face 
  [{:keys [csize at]}]
  [(+ (quot (at :x) csize) (* (quot (at :y) csize) 4))
   (mod (at :x) csize)
   (mod (at :y) csize)
   (- csize 1 (mod (at :x) csize))
   (- csize 1 (mod (at :y) csize))])

(defn at-face 
  [csize n {:keys [x y]}]
  (condp = n
    ; yes, yes, I know
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
    12 {:x (+ x) :y (+ csize csize csize y)}))

(defn face-wrap
  "Manual wrapper-fn for test case"
  [_drawing {:keys [csize] :as state}]
  (let [[face rx ry invx invy] (current-face state)]
    (condp = [face (:facing state)]
      [2 :N] (-> state
               (assoc :facing :S)
               (assoc :at (at-face csize 2 {:x invx, :y 0})))
      [4 :N] (-> state
               (assoc :facing :S)
               (assoc :at (at-face csize 4 {:x invx :y 0})))
      [5 :N] (-> state
               (assoc :facing :E)
               (assoc :at (at-face csize 2 {:x 0 :y rx})))
      [11 :N] (-> state
                (assoc :facing :W)
                (assoc :at (at-face csize 6 {:x invx :y 0})))
      [4 :S] (-> state
               (assoc :facing :N)
               (assoc :at (at-face csize 10 {:x invx, :y ry})))
      [5 :S] (-> state
               (assoc :facing :E)
               (assoc :at (at-face csize 10 {:x 0 :y invx})))
      [10 :S] (-> state
                (assoc :facing :N)
                (assoc :at (at-face csize 4 {:x invx :y ry})))
      [11 :S] (-> state
                (assoc :facing :E)
                (assoc :at (at-face csize 6 {:x 0 :y invx})))
      [2 :W] (-> state
               (assoc :facing :N)
               (assoc :at (at-face csize 5 {:x ry, :y 0})))
      [4 :W] (-> state
               (assoc :facing :N)
               (assoc :at (at-face csize 11 {:x invy :y invx})))
      [10 :W] (-> state
                (assoc :facing :N)
                (assoc :at (at-face csize 5 {:x invy :y invx})))
      [2 :E] (-> state
               (assoc :facing :W)
               (assoc :at (at-face csize 11 {:x invx, :y ry})))
      [6 :E] (-> state
               (assoc :facing :S)
               (assoc :at (at-face csize 11 {:x invy :y 0})))
      [11 :E] (-> state
                (assoc :facing :W)
                (assoc :at (at-face csize 2 {:x invx :y ry}))))))

(defn pube-wrap
  "Manual wrapper-fn for my input. No, I don't want to make a generic cube layouter."
  [_drawing {:keys [csize] :as state}]
  (let [[face rx ry invx invy] (current-face state)
        bottom (dec csize)
        right (dec csize)]
    ;(trace [state face rx ry])
    (condp = [face (:facing state)]
      [1 :N] (-> state
               (assoc :facing :E)
               (assoc :at (at-face csize 12 {:x 0, :y rx})))
      [1 :W] (-> state
               (assoc :facing :E)
               (assoc :at (at-face csize 8 {:x 0, :y invy})))
      [2 :N] (-> state
               (assoc :facing :N)
               (assoc :at (at-face csize 12 {:x rx :y bottom})))
      [2 :E] (-> state
               (assoc :facing :W)
               (assoc :at (at-face csize 9 {:x right :y invy})))
      [2 :S] (-> state
               (assoc :facing :W)
               (assoc :at (at-face csize 5 {:x right :y rx})))
      [5 :W] (-> state
               (assoc :facing :S)
               (assoc :at (at-face csize 8 {:x ry :y 0})))
      [5 :E] (-> state
               (assoc :facing :N)
               (assoc :at (at-face csize 2 {:x ry :y bottom})))
      [8 :N] (-> state
               (assoc :facing :E)
               (assoc :at (at-face csize 5 {:x 0 :y rx})))
      [8 :W] (-> state
               (assoc :facing :E)
               (assoc :at (at-face csize 1 {:x 0 :y invy})))
      [9 :E] (-> state
               (assoc :facing :W)
               (assoc :at (at-face csize 2 {:x right :y invy})))
      [9 :S] (-> state
               (assoc :facing :W)
               (assoc :at (at-face csize 12 {:x right :y rx})))
      [12 :W] (-> state
                (assoc :facing :S)
                (assoc :at (at-face csize 1 {:x ry :y 0})))
      [12 :S] (-> state
                (assoc :facing :S)
                (assoc :at (at-face csize 2 {:x rx :y 0})))
      [12 :E] (-> state
                (assoc :facing :N)
                (assoc :at (at-face csize 9 {:x ry :y bottom}))))))


(def ^:dynamic *wrapper* wrap-pt1)

(defn tile-facing [drawing state test-at]
  (if-let [tile (drawing test-at)]
    [tile (assoc state :at test-at)]
    (let [new-state (*wrapper* drawing state)]
      [(drawing (:at new-state)) new-state])))



(defn take-step [drawing {:keys [facing at] :as state}]
  ;(trace [:step state])
  (let [{:keys [x y]} at
        [tile new-state] (condp = facing
                           :N (tile-facing drawing state {:x x :y (dec y)})
                           :S (tile-facing drawing state {:x x :y (inc y)})
                           :W (tile-facing drawing state {:x (dec x) :y y})
                           :E (tile-facing drawing state {:x (inc x) :y y}))]
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
  (binding [*wrapper* pube-wrap] (apply solve-1 param)))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    [10 :R 5 :L 5 :R 10 :L 4 :R 5 :L 5] (parse-path "10R5L5R10L4R5L5")
    6032 (solve-1 "resources/2022/day22.test.txt")
    5031 (binding [*wrapper* face-wrap] (solve-1 "resources/2022/day22.test.txt"))))
