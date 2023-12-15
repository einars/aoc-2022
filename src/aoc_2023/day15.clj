(ns aoc-2023.day15
  (:require
    [clojure.test :as test :refer [deftest are]]
    [clojure.string :as str]
    [aoc.helpers :as h]))

(def sample-data "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")
(def input-file "resources/2023/day15.txt")

(defn xhash [s]
  (reduce (fn [h n] (mod (* 17 (+ h n)) 256)) 0 (map int s)))

(defn xhm-op-minus [xhm s]
  (let [key (xhash s)]
    (update xhm key #(filter (fn [[k _v]] (not= k s)) %))))

(defn xhm-op-eq [xhm s lens]
  (let [key (xhash s)
        box (xhm key [])
        lens (parse-long lens)]
    (if (some (fn [[k _]] (= k s)) box)
      ; update existing
      (update xhm key #(mapv (fn [[k v]] (if (= k s) [k lens] [k v])) %))
      ; append
      (update xhm key #(cons [s lens] %)))))

(defn xhm-op [xhm s]
  (if (str/ends-with? s "-")
    (xhm-op-minus xhm (first (str/split s #"-")))
    (apply xhm-op-eq xhm (str/split s #"="))))

(defn slot-values [box elts]
  (prn :slot-values box elts)
  (reduce + 
    (map-indexed 
      (fn [idx n] (* (inc box) (inc idx) n)) 
      (reverse (map second elts)))))

(defn score-xhm [xhm]
  (reduce + (map #(apply slot-values %) xhm)))

(defn solve-1
  ([] (solve-1 (str/trim (slurp input-file))))
  ([m] (->> (str/split m #",")
         (mapv xhash)
         (reduce +))))

(defn solve-2
  ([] (solve-2 (str/trim (slurp input-file))))
  ([m] (->> 
         (str/split m #",")
         (reduce xhm-op {})
         score-xhm)))

(deftest test-stuff [] 
  (are [x y] (= x y)
    52 (xhash "HASH") 
    1320 (solve-1 sample-data)
    145 (solve-2 sample-data)))
; 288494 too high


(comment
  (solve-1)
  (solve-2))
