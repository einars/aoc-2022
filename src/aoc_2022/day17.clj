(ns aoc-2022.day17
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))


(def figures
  [[[0 0] [1 0] [2 0] [3 0]]
   [[1 0] [0 1] [1 1] [2 1] [1 2]]
   [[0 0] [1 0] [2 0] [2 1] [2 2]]
   [[0 0] [0 1] [0 2] [0 3]]
   [[0 0] [0 1] [1 0] [1 1]]])

(def initial-board {:board #{} :height 0})

(defn move-fig [[x y] move]
  (condp = move
    \> [(inc x) y]
    \< [(dec x) y]))

(defn may-place 
  "Iterate through the figure coordinates, offset by [dx, dy] 
  and check if they are inside bounds and not already taken on the board"
  [{:keys [board]} fig [dx dy]]
  (not (some (fn [[x y]]
               (let [tx (+ x dx), ty (+ y dy)]
                 (or (< tx 0) (> tx 6) (< ty 0) (board [tx ty]))))
         fig)))

(defn materialize [board figure [dx dy]]
  (let [moved-fig (map (fn [[x y]] [(+ x dx) (+ y dy)]) figure)]
    (-> board
      (update :board #(reduce conj % moved-fig))
      (update :height #(max % (first (sort > (map (comp inc second) moved-fig))))))))

(defn board-signature 
  "String hash of board state: figure data, move index and upper 20 rows of tower"
  [{:keys [board height]} figure move-idx]
  (str (reduce (fn [sgn y] (conj sgn (+ (if (contains? board [0 y]) 1 0)
                                       (if (contains? board [1 y]) 2 0)
                                       (if (contains? board [2 y]) 4 0)
                                       (if (contains? board [3 y]) 8 0)
                                       (if (contains? board [4 y]) 16 0)
                                       (if (contains? board [5 y]) 32 0)
                                       (if (contains? board [6 y]) 64 0))))
         [figure move-idx] (range height (max 0 (- height 20)) -1))))

(def seen (atom {}))

(defn place-fig [[board [figure & rest-figs] moveset]]
  (loop [[move & rest-moves] moveset 
         fig-at [2 (+ 3 (board :height))]]

    (let [side-fig-at (move-fig fig-at move)
          ; horizontal movement, if possible:
          side-fig-at (if (may-place board figure side-fig-at) side-fig-at fig-at) 
          ; try moving down:
          dn-fig-at [(first side-fig-at) (dec (second side-fig-at))]]
      (if (may-place board figure dn-fig-at)
        (recur rest-moves dn-fig-at)
        ; finish, no downward movement:
        [(materialize board figure side-fig-at) rest-figs rest-moves]))))

(defn find-loop [[board [figure & rest-figs] idx-moveset] n]

  (let [signature (board-signature board figure (second (first idx-moveset)))]

    (if-let [[iter-seen, then-height] (@seen signature)]

      (do
        (comment prn "LOOP DETECTED" iter-seen n signature :then-h then-height :now-h (:height board))
        (reduced [(- n iter-seen) (- (:height board) then-height)]))

      (do
        (swap! seen #(assoc % signature [n (:height board)]))
        (loop [[[move _idx] & rest-moves] idx-moveset 
               fig-at [2 (+ 3 (board :height))]]

          (let [side-fig-at (move-fig fig-at move)
                side-fig-at (if (may-place board figure side-fig-at) side-fig-at fig-at)
                dn-fig-at [(first side-fig-at) (dec (second side-fig-at))]]
            (if (may-place board figure dn-fig-at)
              (recur rest-moves dn-fig-at)
              [(materialize board figure side-fig-at) rest-figs rest-moves])))))))

(defn detect-loop [figures moveset]
  (reset! seen {})
  (let [indexed-moveset (cycle (h/zip moveset (iterate inc 1)))]
    (reduce find-loop [initial-board (cycle figures) indexed-moveset] (range))))

(defn get-height [figures moveset n]
  (let [game-stream (iterate place-fig [initial-board (cycle figures) (cycle moveset)])]
    (if-let [[loop-len height-delta] (detect-loop figures moveset)]
      (let [n-loops (quot n loop-len)
            new-n (mod n loop-len)]
        (+ (:height (first (nth game-stream new-n)))
          (* n-loops height-delta)))
      (:height (first (nth game-stream n))))))


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
