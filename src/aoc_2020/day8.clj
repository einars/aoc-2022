(ns aoc-2020.day8
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defrecord State [code pc acc visited])

(defn initial-state [code] 
  (State. code 0 0 #{}))

(defn parse [cmd]
  (let [[cmd n] (str/split cmd #" ")]
    [(keyword cmd) (Integer/parseInt n)]))

(defn exec [state]
  (let [[cmd n] (get (:code state) (:pc state))
        new-state (update state :visited #(conj % (:pc state)))
        new-state (condp = cmd
                    :nop (update new-state :pc inc)
                    :acc (-> new-state
                           (update :acc #(+ n %))
                           (update :pc inc))
                    :jmp (update new-state :pc #(+ n %)))]
    new-state))

(defn terminal? [state]
  (or
    (>= (:pc state) (count (:code state)))
    (get (:visited state) (:pc state))))

(defn modifications-seq 
  "Returns sequence of program code with one nop changed to jmp or jmp to nop"
  [code]
  (concat
    (for [[idx [cmd n]] (h/indexed code) :when (= cmd :nop)]
      (assoc code idx [:jmp n]))
    (for [[idx [cmd n]] (h/indexed code) :when (= cmd :jmp)]
      (assoc code idx [:nop n]))))

(defn run-until-completion [code]
  (->> code
    initial-state
    (iterate exec)
    (drop-while (complement terminal?))
    first))

(defn solve-1
  ([] (solve-1 "resources/2020/day8.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (mapv parse)
     initial-state
     (iterate exec)
     (drop-while (complement terminal?))
     (first)
     :acc)))

(defn solve-2
  ([] (solve-2 "resources/2020/day8.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (mapv parse)
     modifications-seq
     (map run-until-completion)
     (drop-while (fn [final-st] (get (:visited final-st) (:pc final-st))))
     (first)
     :acc)))

;(solve-1 "resources/2020/day8.test.txt")
;(solve-2 "resources/2020/day8.test.txt")

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    5 (solve-1 "resources/2020/day8.test.txt")
    8 (solve-2 "resources/2020/day8.test.txt")))
