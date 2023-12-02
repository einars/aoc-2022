(ns aoc-2023.day2
  (:require
    [clojure.test :as test :refer [deftest is are]]
    [protoflex.parse :as p :refer (sep-by parse series integer word string-in)]
    [aoc.helpers :as h]))

(def ^:dynamic *initial-bag* {:red 12
                              :green 13
                              :blue 14 })


(def sample-games ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
                   "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
                   "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
                   "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
                   "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" ])

(defn parse-color [] (string-in ["red" "blue" "green"]))

(defn parse-reveal [] 
  (sep-by #(series integer parse-color) #(p/chr \,) #(p/chr \;)))

(defn process-reveals [rs]
  (->> rs 
    (mapv (fn [[a b]] [(keyword b) a]))
    (into {})))

(defn parse-reveals []
  (->> (p/multi+ parse-reveal)
    (mapv process-reveals)))

(defn parse-game []
  (let [[_ game _ reveals] (series #(word "Game") integer #(word ":") parse-reveals)]
    {:game game
     :reveals reveals}))
  
;(parse parse-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

(defn string-to-game [l] (parse parse-game l))

(defn reveal-possible? [reveal]
  (every? (fn [[color count]] (>= (color *initial-bag*) count)) reveal))

(defn game-possible? [game]
  (every? reveal-possible? (:reveals game)))

(defn get-minimum-score [{:keys [reveals]}]
  (*
    (apply max (filter some? (map :red reveals)))
    (apply max (filter some? (map :blue reveals)))
    (apply max (filter some? (map :green reveals)))))


(defn solve-1 
  ([] (solve-1 (h/slurp-strings "resources/2023/day2.txt")))
  ([lines]
   (->> lines
     (mapv string-to-game)
     (filter game-possible?)
     (mapv :game)
     (reduce +))))


(defn solve-2
  ([] (solve-2 (h/slurp-strings "resources/2023/day2.txt")))
  ([lines]
   (->> lines
     (mapv string-to-game)
     (mapv get-minimum-score)
     (reduce +))))

(deftest tests []
  (are [x y] (= x y)
    {:game 1, :reveals [{:blue 3 :red 4}
                        {:red 1 :green 2 :blue 6}
                        {:green 2}]}
    (string-to-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
    8 (solve-1 sample-games)
    2286 (solve-2 sample-games)))

(comment
  (solve-1)
  (solve-2))

