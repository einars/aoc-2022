(ns aoc-2023.day4
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [instaparse.core :as insta]
    [aoc.helpers :as h]))

(def sample-data [
                  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
                  "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
                  "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
                  "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
                  "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
                  "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"])

(def card-parser (insta/parser
                   "
    <game> = <'Card'> <ws> int <':'> <ws> numbers <ws> <'|'> <ws> numbers
    numbers = int (<ws> int)*
    <int> = #'\\d+'
    <ws> = #' +'
    "))

(defn parse-card [s]
  (let [[n n1 n2] (card-parser s)]
    {:game (parse-long n)
     :n1 (set (mapv parse-long (drop 1 n1)))
     :n2 (set (mapv parse-long (drop 1 n2)))}))

(defn card-score [c]
  (let [matches (count (set/intersection (:n1 c) (:n2 c)))]
    (condp = matches
      0 0
      1 1
      (reduce * (repeat (dec matches) 2)))))

(defn n-matches [c]
  (count (set/intersection (:n1 c) (:n2 c))))

(defn mark [extra-copies base-idx score]
  (let [
        cur-cards (inc (get extra-copies base-idx 0))
        new-copies (update extra-copies  
                     (+ base-idx score) 
                     #(+ (or % 0) cur-cards))]
    (if (= 1 score)
      new-copies
      (recur new-copies base-idx (dec score)))))

(defn run-bun [[card & rest-cards] extra-copies idx]
  (let [score (n-matches (parse-card card))]
    (if (> score 0)
      (recur rest-cards (mark extra-copies idx score) (inc idx))
      (if (empty? rest-cards) 
        extra-copies
        (recur rest-cards extra-copies (inc idx))))))

(defn solve-1
  ([] (solve-1 (h/slurp-strings "resources/2023/day4.txt")))
  ([m] (->> m
         (mapv parse-card)
         (mapv card-score)
         (reduce +))))

(defn solve-2
  ([] (solve-2 (h/slurp-strings "resources/2023/day4.txt")))
  ([m] 
   (->> (run-bun m {} 0)
     vals
     (reduce +)
     (+ (count m))
     )))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    13 (solve-1 sample-data)
    30 (solve-2 sample-data)
    ))

(comment
  (solve-1)
  (solve-2))
