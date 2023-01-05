(ns aoc-2020.day14
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as string]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn extract-mask [s]
  (let [[bits] (re-seq #"[01X]+$" s)
        bits (h/indexed (reverse bits))]
    (keep #(condp = (second %)
             \1 [(first %) 1]
             \0 [(first %) 0]
             \X nil) bits)))

(defn parse-mem [s]
  (let [[[_ addr data]] (re-seq #"^mem\[(\d+)\] = (\d+)$" s)]
    {:cmd :mem :addr (Integer/parseInt addr) :data (Integer/parseInt data)}))

(defn parse [s]
  (cond
    (string/starts-with? s "mask =") {:cmd :mask :mask (extract-mask s)}
    (string/starts-with? s "mem") (parse-mem s)))

(defn masked [n mask] 
  (reduce (fn [n [bit q]]
            (if (zero? q)
              (bit-clear n bit)
              (bit-set n bit))) n mask))

(defn exec [state cmd]
  (condp = (:cmd cmd)
    :mask (assoc state :mask (cmd :mask))
    :mem (update state :mem #(assoc % (:addr cmd) (masked (:data cmd) (:mask state))))))


(defn solve-1
  ([] (solve-1 "resources/2020/day14.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (map parse)
     (reduce exec {:mask [] :mem {}})
     :mem
     vals
     (reduce +))))


(deftest test-stuff [] 
  (test/are [x y] (= x y)
    165 (solve-1 "resources/2020/day14.test.txt")))
