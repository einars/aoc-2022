(ns aoc-2022.day21
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn parse-math [m] 
  (try
    (Integer/parseInt m)
    (catch Exception _
      (let [[_ a op b] (first (re-seq #"([a-z]+) ([+\-*\/]+) ([a-z]+)" m))]
        [(symbol op) (keyword a) (keyword b)]))))

(defn parse [s]
  (let [[name rest] (str/split s #": ")]
    [(keyword name) (parse-math rest)] ) )

(def monkey-eval)

(defn monkey-eval-impl [what all]
  (let [x (all what)]
    (cond
      (number? x) x
      :else 
      (eval (list (first x) (monkey-eval (second x) all) (monkey-eval (nth x 2) all))) )) )


(def monkey-eval (memoize monkey-eval-impl))

(defn solve-1
  ([] (solve-1 "resources/2022/day21.txt"))
  ([file]
   ( ->>
     (h/slurp-strings file)
     (map parse)
     (into {})
     (monkey-eval :root))))

;(solve-1 "resources/2022/day21.test.txt")
(solve-1)

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    152 (solve-1 "resources/2022/day21.test.txt")
    ))
