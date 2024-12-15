(ns aoc-2024.day15
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [clojure.tools.trace :refer :all]
   [instaparse.core :as insta]
   [aoc.helpers :as h]))

(def input-txt "resources/2024/day15.txt")

(def ^:dynamic *part* 1)

(def sample
  "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
")

(defn parse-input
  [s]
  (let [[area moves] (str/split s #"\n\n")
        area (h/string->map area)
        robot (first (h/find-keys #{\@} area))
        boxes (set (h/find-keys #{\O} area))
        walls (set (h/find-keys #{\#} area))

        moves (str/replace moves #"\n" "")]

    [walls robot boxes moves]))

(defn prepare-part-2 [walls robot boxes moves]
  (let [w1 (mapv (fn [c]  (update c :x #(* % 2))) walls)
        w2 (mapv (fn [c] (update c :x #(inc (* % 2)))) walls)
        walls (set (concat w1 w2))
        robot (update robot :x #(* 2 %))
        boxes (set (mapv (fn [c] (update c :x #(* % 2))) boxes))]
    [walls robot boxes moves]))

(def move-fns
  {\< h/left-of
   \> h/right-of
   \^ h/top-of
   \v h/bottom-of})

(defn slurp-boxes
  [pos move walls boxes] 
  (let [dir-fn (move-fns move)]
    (loop [pos (dir-fn pos) accum []]
      (cond 
        (walls pos) nil
        (boxes pos) (recur (dir-fn pos) (conj accum pos))
        :else accum))))

(defn debug-pt-2 [robot walls boxes]
  (let [mm {robot "@"}
        mm (reduce (fn [mm w] (assoc mm w "#")) mm walls)
        mm (reduce (fn [mm w] (assoc mm w "[")) mm boxes)]
    (h/print-map mm)))

(defn slurp-boxes-pt2
  ([pos move walls boxes] (slurp-boxes-pt2 pos move walls boxes []))
  ([pos move walls boxes accum] 
   (let [dir-fn (move-fns move)]
     (loop [pos (dir-fn pos) accum accum]
       (cond 
         (walls pos) nil

         (#{\<} move)
         (do
           (if (boxes (h/left-of pos))
             (recur (h/left-of (h/left-of pos)) (conj accum (h/left-of pos)))
             accum))

         (#{\>} move)
         (if (boxes pos)
           (recur (h/right-of (h/right-of pos)) (conj accum pos))
           accum)

         :else
         (let [b1 (if (boxes pos) (slurp-boxes-pt2 pos move walls boxes [pos]) accum)
               b2 (if (boxes pos) (slurp-boxes-pt2 (h/right-of pos) move walls boxes [pos]) accum)
               b3 (if (boxes (h/left-of pos)) (slurp-boxes-pt2 (h/left-of pos) move walls boxes [(h/left-of pos)]) accum)
               b4 (if (boxes (h/left-of pos)) (slurp-boxes-pt2 pos move walls boxes [(h/left-of pos)]) accum)]
           (if (or (nil? b1) (nil? b2) (nil? b3) (nil? b4))
             nil
             (set (concat b1 b2 b3 b4)))))))))


(defn apply-move [move robot walls boxes]
  (let [dir-fn (move-fns move)
        moved-boxes (cond
                      (= *part* 1) (slurp-boxes robot move walls boxes)
                      (= *part* 2) (slurp-boxes-pt2 robot move walls boxes))]

    (if (nil? moved-boxes)
      [robot boxes]
      [(dir-fn robot) (apply conj
                        (apply disj boxes moved-boxes)
                        (map dir-fn moved-boxes))])))

(defn apply-moves [moves robot walls boxes]
  (loop [moves moves, robot robot, boxes boxes]
    (if (empty? moves)
      [robot boxes]
      (let [[robot boxes] (apply-move (first moves) robot walls boxes)]
        (recur (rest moves) robot boxes)))))

(defn box-score [boxes]
  (reduce + (map (fn [{:keys [x y]}] (+ x (* 100 y))) boxes)))


(defn pt1
  [task]
  (let [[w r b m] task
        [r b] (apply-moves m r w b)]
    (box-score b)))

(defn pt2
  [task]
  (binding [*part* 2]
    (let [[w r b m] task
          [w r b m] (prepare-part-2 w r b m)
          [r b] (apply-moves m r w b)]
      (box-score b))))

(defn solve-1
  ([] (solve-1 (slurp input-txt)))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp input-txt)))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    10092 (pt1 (parse-input sample))
    9021 (pt2 (parse-input sample))))

(comment
  (solve-1)
  (solve-2))
