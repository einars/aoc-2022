(ns aoc-2022.day5
  (:require [clojure.test :as test :refer [deftest]])
  (:require [clojure.string :as str])
  (:require [aoc-2022.helpers :as h]))

(defn parse-boxes
  [bs]
  (let [transposed (apply mapv vector (rseq bs))
        box-defs (for [x (range 1 (count transposed) 4)] (nth transposed x))
        box-names (mapv first box-defs)
        box-names (mapv #(Integer/parseInt (str %)) box-names)
        box-vals (mapv #(filter (partial not= \space) (reverse (rest %))) box-defs)
    ]
    (zipmap box-names box-vals)))

(defn parse-move
  [move-s]
  (let [[_ n src dst] (first (re-seq #"move (\d+) from (\d+) to (\d+)" move-s))]
    {:src (Integer/parseInt src), :dst (Integer/parseInt dst), :n (Integer/parseInt n)}))

(defn apply-move
  [boxes move]
  (let [{:keys [src dst n]} move
        transfer (take n (boxes src))]
    (-> boxes
      (assoc src (drop n (boxes src)))
      (assoc dst (concat (reverse transfer) (boxes dst))))))

(defn solve-1
  ([] (solve-1 "resources/day5.txt"))
  ([file]
    (let [[boxes moves] (h/slurp-blocks file)
          boxes (parse-boxes boxes)
          moves (mapv parse-move moves)
          boxes (reduce apply-move boxes moves)
      ]
      (str/join (map first (vals (into (sorted-map) boxes)))))))

(defn apply-move-no-reverse
  [boxes move]
  (let [{:keys [src dst n]} move
        transfer (take n (boxes src))]
    (-> boxes
      (assoc src (drop n (boxes src)))
      (assoc dst (concat transfer (boxes dst))))))

(defn solve-2
  ([] (solve-2 "resources/day5.txt"))
  ([file]
    (let [[boxes moves] (h/slurp-blocks file)
          boxes (parse-boxes boxes)
          moves (mapv parse-move moves)
          boxes (reduce apply-move-no-reverse boxes moves)
      ]
      (str/join (map first (vals (into (sorted-map) boxes)))))))


(deftest test-stuff [] 
  (test/are [x y] (= x y)
    {:src 3, :dst 9, :n 7} (parse-move "move 7 from 3 to 9")
    "CMZ" (solve-1 "resources/day5.test.txt")
    "MCD" (solve-2 "resources/day5.test.txt")
    ))

