(ns aoc-2016.day2
  (:require [clojure.test :as test :refer [deftest]])
  (:require [clojure.string :as str])
  (:require [clojure.tools.trace :as trace :refer[deftrace trace]])
  (:require [clojure.pprint :as pprint])
  (:require [aoc.helpers :as h]))

(defn tpl-get-in
  "same as get-in, but returns nil on \\space"
  [m ks]
  (let [ret (get-in m ks)]
    (when (not= ret \space) ret)))


(defn template->moves
  [template]
  (into {}
    (for [x (range 5)
          y (range 5)
          :let [ch (get-in template [y x])]
          :when (not= \space ch)]

      [ch {\U (tpl-get-in template [(dec y) x])
           \D (tpl-get-in template [(inc y) x])
           \L (tpl-get-in template [y (dec x)])
           \R (tpl-get-in template [y (inc x)])}]
      )))

(def moves-1 (template->moves ["     " " 123 " " 456 " " 789 " "     "]))
(def moves-2 (template->moves ["  1  " " 234 " "56789" " ABC " "  D  "]))

(def ^:dynamic *moveset* moves-1)

(defn move 
  "ex: (move \5 \\U)"
  [what dir]
  (if-some [res (get (*moveset* what) dir)]
    res
    what))


(defn walk-code
  "ex: (walk-code \5 [\\U \\L \\L])"
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
   (binding [*moveset* moves-1]
     (->>
       (h/slurp-strings file)
       (walk-codes \5)
       str/join))))

(defn solve-2
  ([] (solve-2 "resources/2016/day2.txt"))
  ([file]
   (binding [*moveset* moves-2]
     (->>
       (h/slurp-strings file)
       (walk-codes \5)
       str/join))))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    \2 (move \5 \U)
    \1 (move \1 \L)
    \1 (walk-code \5 "ULL")
    [\1 \9] (walk-codes \5 ["ULL", "RRDDD"])
    "1985" (solve-1 "resources/2016/day2.test.txt")
    "5DB3" (solve-2 "resources/2016/day2.test.txt")
    ))
