(ns aoc-2024.day16
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
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
  ([start end walls] (first (sort (vals (get (trace-paths start :rt end walls) end)))))
  ([pos facing target walls]

   (loop [[x & xs :as front] #{{:pos pos :facing facing :score 0}}, new-front #{}, known-best {} ]


     (if (empty? front)
       (if (empty? new-front)
         known-best
         (recur new-front #{} known-best))

       (let [pos (:pos x)
             facing (:facing x)
             score (:score x)]

         (if (< (get-in known-best [pos facing] 9999999999) score)
           (recur xs new-front known-best)

           (let [known-best (assoc-in known-best [pos facing] score)
                 new-front (if (walls (h/move pos facing)) 
                             new-front 
                             (conj new-front {:pos (h/move pos facing) 
                                              :facing facing 
                                              :score (inc score)}))
                 new-front (if (walls (h/move pos (turn-right facing)))
                             new-front
                             (conj new-front {:pos (h/move pos (turn-right facing)) 
                                              :facing (turn-right facing) 
                                              :score (+ score 1001)}))
                 new-front (if (walls (h/move pos (turn-left facing)))
                             new-front
                             (conj new-front {:pos (h/move pos (turn-left facing)) 
                                              :facing (turn-left facing) 
                                              :score (+ score 1001)}))]
             (recur xs new-front known-best))))))))


(defn pt1
  [[start end walls]]
  (trace-paths start end walls))

(pt1 (parse-input sample))

(defn pt2
  [task]
  2)

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
