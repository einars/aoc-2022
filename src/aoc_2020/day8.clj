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

(defn solve-1
  ([] (solve-1 "resources/2020/day8.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (mapv parse)
     initial-state
     (iterate exec)
     (drop-while (fn [st] (not (get (:visited st) (:pc st)))))
     (first)
     :acc)))

(solve-1 "resources/2020/day8.test.txt")

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    5 (solve-1 "resources/2020/day8.test.txt")))
