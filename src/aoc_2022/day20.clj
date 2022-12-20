(ns aoc-2022.day20
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn make-indexed [xs] (vec (for [idx (range (count xs))] {:idx idx :val (xs idx)})))
(defn deindex [ms] (mapv :val (sort-by :idx ms)))

(defn shift-back
  [ms from to]
  (mapv (fn [{:keys [idx val] :as v}]
          (cond
            (< (dec to) idx from) {:idx (inc idx) :val val}
            (= idx from) {:idx to :val val}
            :else v)) ms))

(defn shift-fwd
  [ms from to]
  (mapv (fn [{:keys [idx val] :as v}]
          (cond
            (< from idx (inc to)) {:idx (dec idx) :val val}
            (= idx from) {:idx to :val val}
            :else v)) ms))


(defn mix-stream [xs]
  (loop [ms (make-indexed xs), base-idx 0]

    (if (>= base-idx (count ms))
      (deindex ms)
      (let [v (:val (ms base-idx))
            cur-index (:idx (ms base-idx))
            new-index (mod (+ cur-index (:val (ms base-idx))) (dec (count ms)))
            new-index (if (and (= 0 new-index) (< v 0)) (dec (count ms)) new-index) ; move over to the end
            new-index (if (and (= (dec (count ms)) new-index) (> v 0)) 0 new-index) ; move over to the beginning
            ]
        (cond 
          (= cur-index new-index) (recur ms (inc base-idx))
          (> cur-index new-index) (recur (shift-back ms cur-index new-index) (inc base-idx))
          (< cur-index new-index) (recur (shift-fwd ms cur-index new-index) (inc base-idx)))))))

(defn solve-1
  ([] (solve-1 "resources/2022/day20.txt"))
  ([file]
   (let [stream (->>
                  (h/slurp-strings file)
                  (mapv #(Integer/parseInt %))
                  (mix-stream)
                  cycle)

         s (next (drop-while #(not= 0 %) stream))
         a1 (first (drop 999 s))
         a2 (first (drop 1999 s))
         a3 (first (drop 2999 s))]
     (+ a1 a2 a3))))

;(solve-1)

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    [1 2 3 4 0] (deindex (shift-fwd (make-indexed [0 1 2 3 4]) 0 4))
    [4 0 1 2 3] (deindex (shift-back (make-indexed [0 1 2 3 4]) 4 0))
    3 (solve-1 "resources/2022/day20.test.txt")
    ))
