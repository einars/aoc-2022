(ns aoc-2024.day16
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.tools.trace :refer :all]
   [instaparse.core :as insta]
   [aoc.helpers :as h]))

(def input-txt "resources/2024/day16.txt")

(def sample
  "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############")

(def sample-2
  "#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################")

(defn parse-input
  [s]
  (let [m (h/string->map s)
        start (first (h/find-keys #{\S} m))
        end (first (h/find-keys #{\E} m))
        walls (set (h/find-keys #{\#} m))]
    [start end walls]))

(def turn-right 
  {:lt :up
   :up :rt
   :rt :dn
   :dn :lt})

(def turn-left 
  {:lt :dn
   :dn :rt
   :rt :up
   :up :lt})

(defn trace-paths 
  ([start end walls] (first (sort-by :score (vals (get (trace-paths start :rt end walls) end)))))
  ([pos facing target walls]

   (loop [[x & xs :as front] #{{:pos pos :facing facing :score 0 :path #{pos}}}, new-front #{}, known-best {} ]

     (if (empty? front)
       (if (empty? new-front)
         known-best
         (recur new-front #{} known-best))

       (let [{:keys [pos facing score path]} x]

         (if (< (get-in known-best [pos facing :score] 9999999999) score)
           (recur xs new-front known-best)

           (let [known-best (assoc-in known-best [pos facing :score] score)
                 known-best (if (= target pos)
                              (update-in known-best [pos facing :path score ] conj path)
                              known-best)

                 new-front (if (walls (h/move pos facing)) 
                             new-front 
                             (conj new-front {:pos (h/move pos facing) 
                                              :facing facing 
                                              :score (inc score)
                                              :path (conj path (h/move pos facing))
                                              }))
                 new-front (if (walls (h/move pos (turn-right facing)))
                             new-front
                             (conj new-front {:pos (h/move pos (turn-right facing)) 
                                              :facing (turn-right facing) 
                                              :score (+ score 1001)
                                              :path (conj path (h/move pos (turn-right facing)))
                                              }))
                 new-front (if (walls (h/move pos (turn-left facing)))
                             new-front
                             (conj new-front {:pos (h/move pos (turn-left facing)) 
                                              :facing (turn-left facing) 
                                              :score (+ score 1001)
                                              :path (conj path (h/move pos (turn-left facing)))
                                              }))]
             (recur xs new-front known-best))))))))


(defn pt1
  [[start end walls]]
  (:score (trace-paths start end walls)))

(defn pt2
  [[start end walls]]
  (let [res (trace-paths start end walls)
        score (:score res)
        paths (apply set/union (get-in res [:path score]))]
    (count paths)))

(defn debug [[start end walls]]

  (let [res (trace-paths start end walls)
        score (:score res)
        all-paths (get-in res [:path score])
        path (apply set/union all-paths)
        m {}
        m (reduce (fn [m x] (assoc m x "#")) m walls)
        m (reduce (fn [m x] (assoc m x "O")) m path)]

    (spit "/home/e/temp/map.txt" (h/repr-map m))))

(debug (parse-input (slurp input-txt)))

(defn solve-1
  ([] (solve-1 (slurp input-txt)))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp input-txt)))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    7036 (pt1 (parse-input sample))
    2 (pt2 (parse-input sample))))

(comment
  (solve-1)
  (solve-2))
