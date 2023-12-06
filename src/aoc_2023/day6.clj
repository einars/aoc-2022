(ns aoc-2023.day6
  (:require
    [clojure.test :as test :refer [deftest are]]
    [clojure.string :as str]
    [aoc.helpers :as h]))

(def input-file "resources/2023/day6.txt")

(def sample-input ["Time:      7  15   30"
                   "Distance:  9  40  200"])

(defn parse-1 [lines]
  (->> lines
    (take 2)
    (map #(-> % (str/split #":") second str/trim (str/split #" +")))
    (map #(map parse-long %))))

(defn parse-2 [lines]
  (->> lines
    (take 2)
    (map #(-> % (str/split #":") second (str/replace " " "") parse-long))))

(defn beats? [hold-time time dist]
  (> (* hold-time (- time hold-time)) dist))

(defn solve-boat [time dist]
  (reduce 
    (fn [score t] (+ score (if (beats? t time dist) 1 0)))
    0 
    (range 1 time)))

(defn solve-1
  ([] (solve-1 (h/slurp-strings input-file)))
  ([m] (->>
         (parse-1 m)
         (apply mapv solve-boat)
         (reduce *))))

(defn solve-2
  ([] (solve-2 (h/slurp-strings input-file)))
  ([m] (apply solve-boat (parse-2 m))))

(deftest test-stuff [] 
  (are [x y] (= x y)
    288 (solve-1 sample-input)
    71503 (solve-2 sample-input)))

(comment
  (solve-1)
  (solve-2))
