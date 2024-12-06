(ns aoc-2024.day6
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [aoc.helpers :as h]))

(def input "resources/2024/day6.txt")

(def sample
  (str/trim "
  ....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."))

(defn parse-input [s]
  (let [[area dimensions] (-> s (str/split #"\n") h/make-xy-map)
        guard-pos (first (filter (comp #{\^} area) (keys area)))]
    {:dimensions dimensions
     :guard-pos guard-pos
     :direction :up
     :visited #{guard-pos}
     :obstcacles (set (keys (dissoc area guard-pos)))
     }))

(parse-input sample)

(def turn-right 
  {:up :rt
   :rt :dn
   :dn :lt
   :lt :up})

(defn went-outside?
  [{:keys [dimensions] :as task} {:keys [x y] :as pos}]


  )

(defn walk-until-outside
  [{:keys [direction guard-pos obstacles visited] :as task}]

  (let [npos (h/move guard-pos direction)]
    (if (obstacles npos)
      (recur (update task :direction turn-right))

      (if (went-outside? task npos)
        visited
        (recur (-> task
                 (assoc :guard-pos npos)
                 (update :visited #(conj % npos))))))))

(defn pt1
  [task]
  0)

(defn pt2
  [task]
  0)

(defn solve-1
  ([] (solve-1 (slurp input)))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp input)))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    1 (pt1 (parse-input sample))
    2 (pt2 (parse-input sample))))

(comment
  (solve-1)
  (solve-2))
