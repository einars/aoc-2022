(ns aoc-2016.day1
  (:require [clojure.test :as test :refer [deftest]])
  (:require [clojure.string :as str])
  (:require [clojure.tools.trace :as trace])
  (:require [aoc.helpers :as h]))

(def rt [1 0])
(def lt [-1 0])
(def up [0 1])
(def dn [0 -1])

(def turn-left {up lt, lt dn, dn rt, rt up })
(def turn-right {up rt, rt dn, dn lt, lt up })

(defn parse-move
  [move]
  [(first move) (repeat (Integer/parseInt (str/join (rest move))) \F) ])

(defn follow
  [trail] 
  (loop [trail (flatten trail), pos [0 0], facing up]
    (condp = (first trail)
      \F (let [new-pos (map + pos facing)]
        (recur (rest trail) new-pos facing))
      \L (recur (rest trail) pos (turn-left facing))
      \R (recur (rest trail) pos (turn-right facing))
      nil pos)))

(defn follow-until-seen
  [trail] 
  (loop [trail (flatten trail), pos [0 0], facing up, seen #{pos}]
    (condp = (first trail)
      \F (let [new-pos (map + pos facing)]
            (if (seen new-pos)
              new-pos
              (recur (rest trail) new-pos facing (conj seen new-pos))))
      \L (recur (rest trail) pos (turn-left facing) seen)
      \R (recur (rest trail) pos (turn-right facing) seen)
      nil nil)))

(defn solve-1
  ([] (solve-1 "resources/2016/day1.txt"))
  ([file]
    (->>
      (str/split (str/trim (slurp file)) #", ")
      (map parse-move)
      follow
      (map abs)
      (reduce +))))

(defn solve-2
  ([] (solve-2 "resources/2016/day1.txt"))
  ([file]
    (->>
      (str/split (str/trim (slurp file)) #", ")
      (map parse-move)
      follow-until-seen
      (map abs)
      (reduce +))))

(def r2 [\R \F \F])

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    [\L \F \F \F \F \F] (flatten (parse-move "L5"))
    [0 -2] (follow [r2 r2 r2])
    [0 0] (follow [r2 r2 r2 r2])
    nil (follow-until-seen [r2 r2 r2])
    [0 0] (follow-until-seen [r2 r2 r2 r2])
    [0 0] (follow-until-seen [r2 r2 r2 r2 r2])
    ))
