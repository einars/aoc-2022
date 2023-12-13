(ns aoc-2023.day12
  (:require
    [clojure.test :as test :refer [deftest are]]
    [clojure.string :as str]
    [aoc.helpers :as h]))

(def input-file "resources/2023/day12.txt")

(def sample-data 
  ["???.### 1,1,3"
   ".??..??...?##. 1,1,3"
   "?#?#?#?#?#?#?#? 1,3,1,6"
   "????.#...#... 4,1,1"
   "????.######..#####. 1,6,5"
   "?###???????? 3,2,1"])

(def ^:dynamic *unfold* identity)

(defn unfold-2 [[d groups]]
  [(str/join "?" (repeat 5 d))
   (str/join "," (repeat 5 groups))])

(defn parse-problem [s]
  (let [[d groups] (str/split s #" ")
        [d groups] (*unfold* [d groups])]
    {:length (count d)
     :empty (vec (keep-indexed (fn [idx item] (when (= item \.) idx)) d))
     :filled (vec (keep-indexed (fn [idx item] (when (= item \#) idx)) d))
     :groups (mapv parse-long (str/split groups #","))}))

(defn contradicts? [start-pos len prob]
  (or
    (> (+ start-pos len) (:length prob))
    (some #(<= start-pos % (+ start-pos len -1)) (:empty prob))
    (some #{(dec start-pos) (+ start-pos len)} (:filled prob))))

(def n-arrangements)

(defn n-arrangements-n
  ([prob] (n-arrangements prob 0 (:groups prob)))
  ([prob start items]
   (if-not (seq items)
     (if (some (fn [n] (when (>= n start) n)) (:filled prob)) 0 1)
     (let [n (first items)]
       (reduce +
         (for [start-pos (range start 
                           (inc (or 
                                  (some (fn [n] (when (>= n start) n)) (:filled prob)) ; next absolutely filled
                                  (- (:length prob) (apply + items) (dec (count items)))))) ; theoretical max
               :when (not (contradicts? start-pos n prob))]
           (n-arrangements prob (+ start-pos n 1) (rest items))))))))

(def n-arrangements (memoize n-arrangements-n))
;(def n-arrangements n-arrangements-n)



(defn solve-1
  ([] (solve-1 (h/slurp-strings input-file)))
  ([m] (->> m
         (map parse-problem)
         (map n-arrangements)
         (apply +)
         )))

(defn solve-2
  ([] (solve-2 (h/slurp-strings input-file)))
  ([m] (binding [*unfold* unfold-2] (solve-1 m))))

(deftest test-stuff [] 
  (are [x y] (= x y)
    10 (n-arrangements (parse-problem "?###???????? 3,2,1"))
    3 (n-arrangements (parse-problem ".??#?????.???????# 4,5,2"))
    21 (solve-1 sample-data)
    525152 (solve-2 sample-data)))

(comment
  (solve-1)
  (solve-2))
