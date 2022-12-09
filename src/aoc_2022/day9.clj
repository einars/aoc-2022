(ns aoc-2022.day9
  (:require [clojure.test :as test :refer [deftest]])
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:require [clojure.tools.trace :refer [trace deftrace]])
  (:require [clojure.pprint :as pp])
  (:require [aoc.helpers :as h]))


; [head tail]
(def blank-rope (repeat 2 [0 0]))
(def blank-rope-pt2 (repeat 10 [0 0]))

(def directions 
  {\U [0 +1]
   \D [0 -1]
   \L [-1 0]
   \R [+1 0]})

(def head [0 0])
(def tail [0 0])

(defn sign [n]
  (cond
    (< n 0) -1
    (> n 0) +1
    :else 0))

(defn adjust-tail 
  [head tail]
  (if-not (some #(> % 1) (map abs (map - head tail)))
    tail
    (->>
      (map - head tail) 
      (map sign)
      (map + tail))))

(defn move-rope [rope dir]
  (let [new-head (map + (first rope) (directions dir))]
    (reduce (fn [acc this-tail] (conj acc (adjust-tail (last acc) this-tail)))
      [new-head]
      (rest rope))))


(defn move-rope-multi 
  [rope [dir amount]]
  (loop [rope rope, amount amount, acc []]
    (if (zero? amount)
      acc
      (let [new-rope (move-rope rope dir)]
        (recur new-rope (dec amount) (conj acc new-rope))))))

(defn run-with-commands
  "returns full-path"
  [rope commands]
  (reduce 
    (fn [path cmd] (concat path (move-rope-multi (last path) cmd)))
    [rope]
    commands))

(defn tail-positions
  [path]
  (set (map last path)))

(defn parse-command
  [cmd]
  [(first cmd) (Integer/parseInt (subs cmd 2))])

(defn solve-1
  ([] (solve-1 "resources/2022/day9.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (map parse-command)
     (run-with-commands blank-rope)
     tail-positions
     count)))

(defn solve-2
  ([] (solve-2 "resources/2022/day9.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (map parse-command)
     (run-with-commands blank-rope-pt2)
     tail-positions
     count)))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    [[4 0] [3 0]] (last (run-with-commands blank-rope [[\R 4]]))
    [[4 4] [4 3]] (last (run-with-commands blank-rope [[\R 4] [\U 4]]))
    [[1 4] [2 4]] (last (run-with-commands blank-rope [[\R 4] [\U 4] [\L 3]]))
    13 (solve-1 "resources/2022/day9.test.txt")
    1 (solve-2 "resources/2022/day9.test.txt")
    36 (solve-2 "resources/2022/day9.test2.txt")
    ))
