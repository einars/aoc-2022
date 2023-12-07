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

(def joker? zero?)

(defn q= [& xs]
  (if (some joker? xs)
    false
    (apply = xs)))

(def rank-5 7)
(def rank-4 6)
(def rank-3-2 5)
(def rank-3 4)
(def rank-2-2 3)
(def rank-2 2)
(def rank-1 1)

(defn score-hand [hand]
  (let [[c1 c2 c3 c4 c5] (sort > hand)]
    (cond
      (q= c1 c2 c3 c4 c5) rank-5
      (q= c1 c2 c3 c4) rank-4
      (q= c2 c3 c4 c5) rank-4
      (and (q= c1 c2) (q= c3 c4 c5)) rank-3-2
      (and (q= c1 c2 c3) (q= c4 c5)) rank-3-2
      (q= c1 c2 c3) rank-3
      (q= c2 c3 c4) rank-3
      (q= c3 c4 c5) rank-3
      (and (q= c1 c2) (q= c3 c4)) rank-2-2
      (and (q= c1 c2) (q= c4 c5)) rank-2-2
      (and (q= c2 c3) (q= c4 c5)) rank-2-2
      (q= c1 c2) rank-2
      (q= c2 c3) rank-2
      (q= c3 c4) rank-2
      (q= c4 c5) rank-2
      :else rank-1)))

(defn add-joker [rank]
  (condp = rank
    rank-1   rank-2   ; nothing    + J = a pair
    rank-2   rank-3   ; a pair     + J = three
    rank-2-2 rank-3-2 ; two pairs  + J = full-house
    rank-3   rank-4   ; three      + J = four
    rank-3-2 rank-4   ; full-house + J = four (won't ever happen)
    rank-5            ; top out at five
    ))

(defn add-jokers [rank n]
  (if (zero? n)
    rank
    (recur (add-joker rank) (dec n))))

(defn count-jokers [hand] (count (filter joker? hand)))

(defn parse-line [s]
  (let [[hand bid] (str/split s #" ")]
    {:hand (map *card-mapping* hand)
     :bid (parse-long bid)}))

(defn score-camel [{:keys [hand] :as camel}]
  (assoc camel
    :cmp (vec (cons
                (add-jokers (score-hand hand) (count-jokers hand))
                hand))))

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
  [& args] 
  (binding [*card-mapping* joker-mapping ] (apply solve-1 args)))


(deftest test-stuff [] 
  (are [x y] (= x y)
    6440 (solve-1 sample-data)
    5905 (solve-2 sample-data)
    250602641 (solve-1)
    251037509 (solve-2)))

