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


(defn parse-problem [s]
  (let [[d groups] (str/split s #" ")]
    {:length (count d)
     :empty (vec (keep-indexed (fn [idx item] (when (= item \.) idx)) d))
     :filled (vec (keep-indexed (fn [idx item] (when (= item \#) idx)) d))
     :groups (mapv parse-long (str/split groups #","))}))

(defn contradicts? [start-pos len prob]
  (or
    (some #(<= start-pos % (+ start-pos len -1)) (:empty prob))
    (some #{(dec start-pos) (+ start-pos len)} (:filled prob))))

(defn all-ok? [accum prob]
  (let [bitmap (set (mapcat (fn [[pos len]] (range pos (+ pos len))) accum))
        res (not (some (complement bitmap) (:filled prob)))]

    (when res
      (prn :check accum :bmp bitmap (:filled prob) :res)
      )
    res))
    

(defn apply-groups 
  ([prob callback] (apply-groups prob callback 0 (:groups prob) []))
  ([prob callback start items accum]
   (if-not (seq items)
     (when (all-ok? accum prob)
       (callback accum))

     (let [n (first items)]
       ;(prn :doseq start :to  )
       (doseq [start-pos (range start (inc (- (:length prob) (apply + items) (dec (count items)))))]
         (when-not (contradicts? start-pos n prob)
           (apply-groups prob callback (+ start-pos n 1) (rest items) (conj accum [start-pos n]))) ) ) ) ) )


(defn n-arrangements [prob-s]
  (let [results (atom [])]
    (apply-groups (parse-problem prob-s) #(swap! results conj %))
    ;@results
    (count @results)))

(defn solve-1
  ([] (solve-1 (h/slurp-strings input-file)))
  ([m] (->> m
         (map n-arrangements)
         (apply +))))

(defn solve-2
  ([] (solve-2 (h/slurp-strings input-file)))
  ([m] (->> m
         )))

(deftest test-stuff [] 
  (are [x y] (= x y)
    10 (n-arrangements "?###???????? 3,2,1")
    21 (solve-1 sample-data)
    0 (solve-2 sample-data)))

(comment
  (solve-1)
  (solve-2))
