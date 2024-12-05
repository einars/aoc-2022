(ns aoc-2024.day5
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.tools.trace :refer :all]
   [instaparse.core :as insta]
   [clojure.string :as str]
   [aoc.helpers :as h]))

(def sample
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
")


(def parser 
  (insta/parser "
<source> = rules <nl> updates
rules = (rule <nl>) *
updates = (update <nl>) *
nl = '\\n'
rule = int <'|'> int
update = int (<','> int) * 
<int> = #'\\d+'
    ")
  )

(defn make-rule [[_ a b]]
  [(parse-long a) (parse-long b)])

(defn make-update [[_ & xs]]
  (mapv parse-long xs))

(defn parse-input
  [s]
  (let [[[_ & rules] [_ & updates]] (parser s)] 
    {:rules (mapv make-rule rules)
     :updates (mapv make-update updates)}))

(defn matches-rule?
  [update [a b]]
  (loop [[x & rest] update, seen-second? false]
    (condp = x
      a (if seen-second? false true)
      b (recur rest true)
      nil true
      (recur rest seen-second?))))

(defn matches-rules? 
  [update rules]
  (not (some #((complement matches-rule?) update %) rules)))

(defn enforce 
  [update [a b]]
  (let [update-without-a (filter #(not= a %) update)]
    (concat
      (take-while #(not= b %) update-without-a)
      [a]
      (drop-while #(not= b %) update-without-a))))

(defn reorder-if-no-match
  [update all-rules]
  (loop [update update, [rule & rs] all-rules]
    (cond
      (nil? rule) update
      (matches-rule? update rule) (recur update rs)
      :else (recur (enforce update rule) all-rules))))

(defn take-middle [update]
  (nth update (/ (dec (count update)) 2)))

(defn pt1
  [{:keys [rules updates]}]
  (->> 
    (filter #(matches-rules? % rules ) updates)
    (mapv take-middle)
    (apply +)))

(defn pt2
  [{:keys [rules updates]}]
  (->> updates
    (filter #((complement matches-rules?) % rules))
    (mapv #(reorder-if-no-match % rules ))
    (mapv take-middle)
    (apply +)))

(defn solve-1
  ([] (solve-1 (slurp "resources/2024/day5.txt")))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp "resources/2024/day5.txt")))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    143 (pt1 (parse-input sample))
    123 (pt2 (parse-input sample))
    true (matches-rule? [1 2 3] [1 2])
    true (matches-rules? [1 2 3] [[1 2] [2 3]])
    false (matches-rule? [1 2 3] [2 1])

    [2 1 3] (enforce [1 2 3] [2 1])
    ))

(comment
  (solve-1)
  (solve-2))
