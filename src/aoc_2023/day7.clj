(ns aoc-2023.day7
  (:require
    [clojure.test :as test :refer [deftest are]]
    [clojure.string :as str]
    [aoc.helpers :as h]))

(def sample-data [
                  "32T3K 765"
                  "T55J5 684"
                  "KK677 28"
                  "KTJJT 220"
                  "QQQJA 483"
                  ])

(def card-mapping
  {\2 2
   \3 3
   \4 4
   \5 5
   \6 6
   \7 7
   \8 8
   \9 9
   \T 10
   \J 11
   \Q 12
   \K 13
   \A 14})

(defn score-hand [[c1 c2 c3 c4 c5]]
  (cond
    (= c1 c2 c3 c4 c5) [:five 7 [c1]]
    (= c1 c2 c3 c4) [:four 6 [c1]]
    (= c2 c3 c4 c5) [:four 6 [c2]]
    (and (= c1 c2) (= c3 c4 c5)) [:full-house 5 [c3 c1]]
    (and (= c1 c2 c3) (= c4 c5)) [:full-house 5 [c1 c4]]
    (= c1 c2 c3) [:three 4 [c1]]
    (= c2 c3 c4) [:three 4 [c2]]
    (= c3 c4 c5) [:three 4 [c3]]
    (and (= c1 c2) (= c3 c4)) [:pairs 3 [c1 c3]]
    (and (= c1 c2) (= c4 c5)) [:pairs 3 [c1 c4]]
    (and (= c2 c3) (= c4 c5)) [:pairs 3 [c2 c4]]
    (= c1 c2) [:two 2 [c1]]
    (= c2 c3) [:two 2 [c2]]
    (= c3 c4) [:two 2 [c3]]
    (= c4 c5) [:two 2 [c4]]
    :else [:one 1 [c1 c2 c3 c4 c5]]))

(comment
  (score-hand [5 4 3 2 1])
  (score-hand [5 5 3 2 1])
  (score-hand [5 4 4 2 1])
  (score-hand [5 4 3 3 1])
  (score-hand [5 4 3 2 2])
  (score-hand [5 5 5 2 1])
  (score-hand [5 4 4 4 1])
  (score-hand [5 4 3 3 3])
  (score-hand [5 5 5 2 2])
  (score-hand [5 5 2 2 2])
  (score-hand [5 5 5 5 1])
  (score-hand [5 5 5 5 5]))

(defn parse-card-sorted [s]
  (sort > (mapv card-mapping s)))

(defn parse-card-unsorted [s]
  (mapv card-mapping s))

(defn parse-line [s]
  (let [[card, bid] (str/split s #" ")]
    {:sorted (parse-card-sorted card)
     :unsorted (parse-card-unsorted card)
     :repr s
     :bid (parse-long bid)}))

(defn score-camel-1 [{:keys [sorted unsorted] :as camel}]
  (let [[name rank _sort-order] (score-hand sorted)]
    (assoc camel
      :name name
      :cmp (vec (concat [rank] unsorted)))))


(def input-file "resources/2023/day7.txt")

(defn solve-1
  ([] (solve-1 (h/slurp-strings input-file)))
  ([m] (->> m
         (map parse-line)
         (map score-camel-1)
         (sort-by :cmp)
         (h/indexed)
         (map (fn [[idx h]] (* (inc idx) (:bid h))))
         (reduce +)
         )))

(defn solve-2
  ([] (solve-2 (h/slurp-strings input-file)))
  ([m] (->> m
         )))

(deftest test-stuff [] 
  (are [x y] (= x y)
    [[13 10 3 3 2] 765] (parse-card "32T3K 765")
    6440 (solve-1 sample-data)
    0 (solve-2 sample-data)))

(comment
  (solve-1)
  (solve-2))
