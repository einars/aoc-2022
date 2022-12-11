(ns aoc-2022.day11
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn parse-numbers 
  [s] 
  (mapv #(Integer/parseInt %) (re-seq #"\d+" s)))

(def parse-number (comp first parse-numbers))

(defn parse-test
  [[div-by when-true when-false]] 
  (let [div-test (parse-number div-by)
        when-true (parse-number when-true)
        when-false (parse-number when-false)]
    (fn [n] (if (zero? (mod n div-test)) when-true when-false))))

(defn parse-operation
  [op]
  (let [operand (subs op 25)]
    (condp = (get op 23)
      \* #(* % (if (= "old" operand) % (Integer/parseInt operand)))
      \+ #(+ % (if (= "old" operand) % (Integer/parseInt operand))))))

(defn parse
  [block]
  {:id (parse-number (first block))
   :items (parse-numbers (second block))
   :operation (parse-operation (nth block 2))
   :test (parse-test (nthnext block 3))})

(defn prepare-monkey 
  [m] 
  [(:id m)
   (assoc m :n 0)])

(defn play-single-throw
  [monkey monkeys item]
  (let [
        new-level (int (/ ((:operation monkey) item) 3))
        new-monkey ((:test monkey) new-level)]
    ; (prn (:id monkey) "inspects" item "new-level" new-level "thrown to" new-monkey )
    (-> monkeys
      (update-in [new-monkey :items] #(conj % new-level))
      (assoc-in [(:id monkey) :items] [])
      (update-in [(:id monkey) :n] inc))))

(defn play-round
  "Assumes that the monkey won't throw to himself"
  ([monkeys] (play-round (keys monkeys) monkeys))
  ([ids monkeys]
   (if (seq ids)
     (let [monkey-id (first ids)
           monkey (monkeys monkey-id)
           new-monkeys (reduce (fn [all-monkeys item] (play-single-throw monkey all-monkeys item)) monkeys (:items monkey))]
       (recur (rest ids) new-monkeys))
     monkeys)))

(defn solve-1
  ([] (solve-1 "resources/2022/day11.txt"))
  ([file]
   (let [monks (->> file
                 (h/slurp-blocks)
                 (map parse)
                 (map prepare-monkey)
                 (into {}))]
     (->> (nth (iterate play-round monks) 20)
       (vals)
       (map :n)
       (sort >)
       (take 2)
       (reduce *)))))

(def monks (->> "resources/2022/day11.test.txt"
             (h/slurp-blocks)
             (map parse)
             (map prepare-monkey)
             (into {})))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    [101 95 7 105] (map :n (vals (nth (iterate play-round monks) 20)))
    10605 (solve-1 "resources/2022/day11.test.txt")))
