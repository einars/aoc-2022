(ns aoc-2016.day2
  (:require [clojure.test :as test :refer [deftest]])
  (:require [clojure.string :as str])
  (:require [clojure.tools.trace :as trace])
  (:require [aoc.helpers :as h]))

(def moves
  {1 {      \R 2   \D 4       }
   2 {      \R 3   \D 5   \L 1}
   3 {             \D 6   \L 2}
   4 {\U 1  \R 5   \D 7       }
   5 {\U 2  \R 6   \D 8   \L 4}
   6 {\U 3         \D 9   \L 5}
   7 {\U 4  \R 8              }
   8 {\U 5  \R 9          \L 7}
   9 {\U 6                \L 8}
  })

(defn move 
  "ex: (move 5 \\U)"
  [what dir]
 (get (moves what) dir what))

(defn walk-code
  "ex: (walk-code 5 [\\U \\L \\L])"
  [starting-digit code]
  (reduce move starting-digit code))

(defn walk-codes
  [starting-digit codes]
  (loop [digit starting-digit codes codes accum []]
    (if (seq codes)
      (let [new-digit (walk-code digit (first codes))]
        (recur new-digit (rest codes) (conj accum new-digit)))
      accum)))

(defn solve-1
  ([] (solve-1 "resources/2016/day2.txt"))
  ([file]
    (->>
      (h/slurp-strings file)
      (walk-codes 5)
      str/join)))

(comment defn solve-2
  ([] (solve-2 "resources/2016/day2.txt"))
  ([file]
    (->>
      (h/slurp-strings file)
      ; ...
      )))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    2 (move 5 \U)
    1 (move 1 \L)
    1 (walk-code 5 "ULL")
    [1 9] (walk-codes 5 ["ULL", "RRDDD"])
    "1985" (solve-1 "resources/2016/day2.test.txt")
    ; 0 (solve-2 "resources/2016/day2.test.txt")
    ))
