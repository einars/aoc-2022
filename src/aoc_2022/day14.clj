(ns aoc-2022.day14
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn make-pair [s]
  (map #(Integer/parseInt %) (str/split s #",")))

(defn make-range [x y]
  (cond
    (= x y) [x]
    (> x y) (range x (dec y) -1)
    (< x y) (range x (inc y) 1)))

(defn trace-line
  [pts next-pt]
  (if (empty? pts) 
    (conj pts next-pt)
    (let [last-pt (last pts)]
      (concat pts (for [x (make-range (first last-pt) (first next-pt))
                        y (make-range (second last-pt) (second next-pt))]
                    [x y])))))

(def ^:dynamic *floating-pos* 10)

(defn next-move
  [s walls sand]
  (let [[x y] s
        d [x (inc y)]
        dl [(dec x) (inc y)]
        dr [(inc x) (inc y)]]
    (cond
      (= y *floating-pos*) (throw (Exception. "boom"))
      (not (or (walls d) (sand d))) d
      (not (or (walls dl) (sand dl))) dl
      (not (or (walls dr) (sand dr))) dr
      :else nil)))


(defn settle
  [walls sand]
  (let [moved-sand (keep #(next-move % walls sand) sand)
        new-sand (into #{} (map #(or (next-move % walls sand) %) sand))]
    (cond
      (empty? moved-sand) sand ; settled
      :else (recur walls new-sand))))

(defn parse-line [s]
  (reduce #(trace-line %1 (make-pair %2)) [] (str/split s #" -> ")))

(defn floating-pos [walls]
  (first (sort > (map second walls))))

(defn iterate-until-settled 
  ([walls] (prn "iterate!") (iterate-until-settled walls #{}))
  ([walls sand]
    (printf ".")
    (flush)
    (if-let [new-sand (try (settle walls (conj sand [500 0])) (catch Exception _ nil))]
     (recur walls new-sand)
     sand)))

(defn solve-1
  ([] (solve-1 "resources/2022/day14.txt"))
  ([file]
   (let [walls (into #{} (mapcat parse-line (h/slurp-strings file)))]
     (binding [*floating-pos* (floating-pos walls)]
       (count (iterate-until-settled walls))))))


(deftest test-stuff [] 
  (test/are [x y] (= x y)
    24 (solve-1 "resources/2022/day14.test.txt")
    ))
