(ns aoc-2023.day7
  (:require
    [clojure.test :as test :refer [deftest are]]
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [aoc.helpers :as h]))

(def input-file "resources/2023/day7.txt")

(def sample-data ["32T3K 765"
                  "T55J5 684"
                  "KK677 28"
                  "KTJJT 220"
                  "QQQJA 483"])

(def ^:dynamic *card-mapping*
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

(def joker-mapping
  {\2 2
   \3 3
   \4 4
   \5 5
   \6 6
   \7 7
   \8 8
   \9 9
   \T 10
   \J 0 ; sic: joker
   \Q 12
   \K 13
   \A 14})

(defn q= [& xs]
  (if (some zero? xs)
    false
    (apply = xs)))

(defn score-hand [[c1 c2 c3 c4 c5]]
  (cond
    (q= c1 c2 c3 c4 c5) [:five 7]
    (q= c1 c2 c3 c4) [:four 6]
    (q= c2 c3 c4 c5) [:four 6]
    (and (q= c1 c2) (q= c3 c4 c5)) [:full-house 5]
    (and (q= c1 c2 c3) (q= c4 c5)) [:full-house 5]
    (q= c1 c2 c3) [:three 4]
    (q= c2 c3 c4) [:three 4]
    (q= c3 c4 c5) [:three 4]
    (and (q= c1 c2) (q= c3 c4)) [:pairs 3]
    (and (q= c1 c2) (q= c4 c5)) [:pairs 3]
    (and (q= c2 c3) (q= c4 c5)) [:pairs 3]
    (q= c1 c2) [:two 2]
    (q= c2 c3) [:two 2]
    (q= c3 c4) [:two 2]
    (q= c4 c5) [:two 2]
    :else [:one 1]))

(defn add-joker [rank]
  (condp = rank
    1 2
    2 4
    3 5
    4 6
    5 6
    7))

(defn add-jokers [rank n]
  (if (zero? n)
    rank
    (add-jokers (add-joker rank) (dec n))))

(defn count-jokers [hand] (count (filter zero? hand)))

(defn parse-card-sorted [s]
  (sort > (mapv *card-mapping* s)))

(defn parse-card-unsorted [s]
  (mapv *card-mapping* s))

(defn parse-line [s]
  (let [[card, bid] (str/split s #" ")]
    {:sorted (parse-card-sorted card)
     :unsorted (parse-card-unsorted card)
     :repr s
     :bid (parse-long bid)}))

(defn score-camel [{:keys [sorted unsorted] :as camel}]
  (let [[name rank] (score-hand sorted)]
    (assoc camel
      :name name
      :cmp (vec (concat [(add-jokers rank (count-jokers unsorted))] unsorted)))))

(defn solve-1
  ([] (solve-1 (h/slurp-strings input-file)))
  ([m] (->> m
         (map parse-line)
         (map score-camel)
         (sort-by :cmp)
         (h/indexed)
         (map (fn [[idx h]] (* (inc idx) (:bid h))))
         (reduce +))))

(defn solve-2
  ([] (solve-2 (h/slurp-strings input-file)))
  ([m] 
   (binding [*card-mapping* joker-mapping ] (solve-1 m))))


(deftest test-stuff [] 
  (are [x y] (= x y)
    6440 (solve-1 sample-data)
    5905 (solve-2 sample-data)))

(comment
  (solve-1)
  (solve-2))
