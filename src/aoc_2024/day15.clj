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

(def move-fns
  {\< h/left-of
   \> h/right-of
   \^ h/top-of
   \v h/bottom-of})

(defn slurp-boxes
  [pos dir-fn walls boxes] 
  (loop [pos (dir-fn pos) accum []]
    (cond 
      (walls pos) nil
      (boxes pos) (recur (dir-fn pos) (conj accum pos))
      :else accum)))


(defn apply-move [move robot walls boxes]
  (let [dir-fn (move-fns move)
        moved-boxes (slurp-boxes robot dir-fn walls boxes)]

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
    (box-score b))
  0)

(defn pt2
  [task]
  0)

(defn solve-1
  ([] (solve-1 (slurp input-txt)))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp input-txt)))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    10092 (pt1 (parse-input sample))
    2 (pt2 (parse-input sample))))

(comment
  (solve-1)
  (solve-2))
