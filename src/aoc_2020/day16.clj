(ns aoc-2020.day16
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.core.logic :as logic :refer [everyg]]
    [clojure.core.logic.fd :as fd]
    [clojure.set :as set]
    [clojure.string :as string]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn parse-constraint [s]
  (let [[[_ name f1 t1 f2 t2]] (re-seq #"^([a-z ]+): (\d+)-(\d+) or (\d+)-(\d+)" s)
        f1 (Integer/parseInt f1)
        f2 (Integer/parseInt f2)
        t1 (Integer/parseInt t1)
        t2 (Integer/parseInt t2)]
    {:name (keyword (string/replace name " " "-")) :test #(or (<= f1 % t1) (<= f2 % t2))}))

(defn parse [s]
  (let [[constraints s] (string/split s #"\n\nyour ticket:\n")
        [my-ticket nearby] (string/split s #"\n\nnearby tickets:\n")]
    {:constraints (map parse-constraint (string/split constraints #"\n"))
     :ticket (h/to-int-list my-ticket)
     :nearby (map h/to-int-list (string/split nearby #"\n"))}))

(defn possible-constraints [n constraints]
  (filter #((:test %) n) constraints))

(defn find-invalid-numbers
  "which numbers in :nearby tickets don't match any constraints?"
  [prob]
  (filter #(empty? (possible-constraints % (:constraints prob))) (reduce concat (:nearby prob))))

(defn valid-ticket?
  "which numbers in :nearby tickets don't match any constraints?"
  [prob ticket]
  (empty? (filter empty? (map #(possible-constraints % (:constraints prob)) ticket))))

(defn keep-valid-tickets [prob]
  (assoc prob :nearby (filter #(valid-ticket? prob %) (:nearby prob))))

(defn choose-fields [prob]
  (assoc prob :fields
    (let [poss (for [digit (range (count (:ticket prob)))]
                 (map #(set (map :name (possible-constraints (nth % digit) (:constraints prob)))) (:nearby prob)))]
      (map #(reduce set/intersection %) poss))))


(defn remove-settled [fields n]
  (mapv #(if (keyword? %) % (let [new (disj % n)] (if (empty? new) n new))) fields))

(defn settle [fields]
  (let [fds (atom fields)]
    (doseq [f fields]
      (when (and (set? f) (= 1 (count f)))
        (swap! fds #(remove-settled % (first f)))))
    (if (seq (filter set? @fds))
      (settle @fds)
      @fds)))

(defn get-final-score [prob]
  (let [defs (h/indexed (settle (:fields prob)))]
    ; get departure-fields and multiply them together
    (reduce * (for [[idx field] defs :when (string/starts-with? (name field) "departure")]
                (nth (:ticket prob) idx)))))

(defn solve-1
  ([] (solve-1 "resources/2020/day16.txt"))
  ([file]
   (->>
     (slurp file)
     parse
     find-invalid-numbers
     (reduce +))))


(defn solve-2
  ([] (solve-2 "resources/2020/day16.txt"))
  ([file]
   (->>
     (slurp file)
     parse
     keep-valid-tickets
     choose-fields
     get-final-score)))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    [1, #{2}] (remove-settled [#{1} #{1, 2}] 1)
    71 (solve-1 "resources/2020/day16.test.txt")
    1 (solve-2 "resources/2020/day16.test.txt")))
