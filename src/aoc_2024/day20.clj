(ns aoc-2024.day20
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.tools.trace :refer :all]
   [instaparse.core :as insta]
   [aoc.helpers :as h]))

(def input-txt "resources/2024/day20.txt")

(def sample
  "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
############### ")

(defn parse-input
  [s]
  (let [m (h/string->map s)
        [start] (h/find-keys #{\S} m)
        [target] (h/find-keys #{\E} m)
        walls (set (h/find-keys #{\#} m))
        dims (h/map-dimensions m)
        ]
    [start target walls dims]))


(defn build-rev-distance
  [start target walls]
  (loop [pos target, distance 0, distances {}]
    (if (= pos start)
      (assoc distances pos distance)

      (let [nbs (map (partial h/move pos) [:up :dn :lt :rt])
            nbs (filter (complement walls) nbs)
            nbs (filter (complement distances) nbs)]

        (recur (first nbs) (inc distance) (assoc distances pos distance))))))

(defn manhattan-offsets [n]
  (remove nil?
    (apply concat 
      (for [dx (range (- n) (inc n))]
        (for [dy (range (- n) (inc n))]
          (when (<= (+ (abs dx) (abs dy)) n)
            [dx dy (+ (abs dx) (abs dy))]))))))


(let [[s t v] (parse-input sample)]
  ((build-rev-distance s t v) s))


(defn find-good-cheats [pos track offsets n]
  (remove nil? 
    (concat
      (for [[dx dy dist] offsets]
        (let [npos {:x (+ (:x pos) dx), :y (+ (:y pos) dy)}]
          (when-let [end-dist (track npos)]
            (let [improvement (- (track pos) end-dist dist)]
              (when (< 0 improvement)
                improvement))))))))

(defn solve [task n cutoff]
  (let [[start target walls] task
        track (build-rev-distance start target walls)
        manhattan (manhattan-offsets n)
        improvements (apply concat (map #(find-good-cheats % track manhattan n) (keys track)))]
    {:ans (count (filter #(<= cutoff %) improvements))
     :debug (sort-by first (map (fn [[k v]] [k (count v)]) (group-by identity improvements)))
     } ))

;(solve (parse-input sample) 2 100)
;(solve (parse-input sample) 20 50)


(defn pt1
  [task]
  (:ans (solve task 2 100)))

(defn pt2
  [task]
  (:ans (solve task 20 100)))

(defn solve-1
  ([] (solve-1 (slurp input-txt)))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp input-txt)))
  ([ss] (pt2 (parse-input ss))))

(comment
  (solve-1)
  (solve-2))
