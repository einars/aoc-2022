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

(def initial-board {:board #{} :height 0})

(defn move-fig [[x y] move]
  (condp = move
    \> [(inc x) y]
    \< [(dec x) y]))

(defn may-place [{:keys [board]} fig [x y]]

  (not (some (fn [[fx fy]]
               (let [tx (+ x fx)
                     ty (+ y fy)]
                 (or (< tx 0)
                   (> tx 6)
                   (< ty 0)
                   (board [tx ty])))) fig)))

(defn materialize [board figure [x y]]
  (let [moved-fig (map (fn [[fx fy]] [(+ x fx) (+ y fy)]) figure)]
    (-> board
      (update :board #(reduce conj % moved-fig))
      (update :height #(max % (first (sort > (map (comp inc second) moved-fig))))))))

(defn board-signature [{:keys [board height]} figure move-idx]

  (str (reduce (fn [sgn y] (conj sgn (+ (if (contains? board [0 y]) 1 0)
                                       (if (contains? board [1 y]) 2 0)
                                       (if (contains? board [2 y]) 4 0)
                                       (if (contains? board [3 y]) 8 0)
                                       (if (contains? board [4 y]) 16 0)
                                       (if (contains? board [5 y]) 32 0)
                                       (if (contains? board [6 y]) 64 0))))
         [figure move-idx ] (range height (max 0 (- height 20)) -1))))

(def seen (atom {}))

(defn place-fig [[board [figure & rest-figs] moveset]]

  (let [fig-x 2
        fig-y (+ 3 (board :height))]

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

(defn find-loop [[board [figure & rest-figs] idx-moveset] n]

  (let [fig-x 2
        fig-y (+ 3 (board :height))
        signature (board-signature board figure (second (first idx-moveset)))]

    (if-let [[iter-seen, then-height] (@seen signature)]

      (do
        ;(prn "LOOP DETECTED" iter-seen n signature :then-h then-height :now-h (:height board))
        (reduced [iter-seen (- n iter-seen) (- (:height board) then-height)]))

      (do
        (swap! seen #(assoc % signature [n (:height board)]))
        (loop [[[move _idx] & rest-moves] idx-moveset 
               fig-at [fig-x fig-y]]

          (let [side-fig-at (move-fig fig-at move)
                side-fig-at (if (may-place board figure side-fig-at) side-fig-at fig-at)
                dn-fig-at [(first side-fig-at) (dec (second side-fig-at))]]
            (if (may-place board figure dn-fig-at)
              (recur rest-moves dn-fig-at)
              [(materialize board figure side-fig-at) rest-figs rest-moves])))))))



(defn detect-loop [figures moveset]
  (let [indexed-moveset (cycle (h/zip moveset (iterate inc 1)))]
    (reset! seen {})
    (reduce find-loop [initial-board figures indexed-moveset] (range))))

(defn get-height [figures moveset n]
  (if-let [[start-loop loop-len height-delta] (detect-loop figures moveset)]
    (let [n-loops-skipped (quot (- n start-loop) loop-len)
          new-n (- n (* n-loops-skipped loop-len))]
      (+ (* n-loops-skipped height-delta)
        (:height (first (nth (iterate place-fig [initial-board figures (cycle moveset)]) new-n)))))
    (:height (first (nth (iterate place-fig [initial-board figures (cycle moveset)]) n)))))


(defn solve-1
  ([] (solve-1 "resources/2022/day17.txt"))
  ([file]
   (let [moveset (str/trim (slurp file))]
     (get-height figures moveset 2022))))

(defn solve-2
  ([] (solve-2 "resources/2022/day17.txt"))
  ([file]
   (let [moveset (str/trim (slurp file))]
     (get-height figures moveset 1000000000000))))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    3068 (solve-1 "resources/2022/day17.test.txt")
    1514285714288 (solve-2 "resources/2022/day17.test.txt")))
