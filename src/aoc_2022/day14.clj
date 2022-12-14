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
(def ^:dynamic *floor-pos* nil)

(defn next-move
  [s walls sand]
  (let [[x y] s
        d [x (inc y)]
        dl [(dec x) (inc y)]
        dr [(inc x) (inc y)]]
    (cond
      (= y *floating-pos*) (throw (Exception. "boom"))
      (= (inc y) *floor-pos*) nil
      (not (or (walls d) (sand d))) d
      (not (or (walls dl) (sand dl))) dl
      (not (or (walls dr) (sand dr))) dr
      :else nil)))


(defn settle
  [walls sand grain]
  (let [new-grain (next-move grain walls sand)]
    (if (nil? new-grain) 
      (conj sand grain)
      (recur walls sand new-grain))))

(defn parse-line [s]
  (reduce #(trace-line %1 (make-pair %2)) [] (str/split s #" -> ")))

(defn floating-pos [walls]
  (first (sort > (map second walls))))

(defn iterate-until-settled 
  ([walls] (iterate-until-settled walls #{}))
  ([walls sand]
   (flush)
   (if-let [new-sand (try (settle walls sand [500 0]) (catch Exception _ nil))]
     (if (sand [500 0])
       sand
       (recur walls new-sand))
     sand)))

(defn solve-1
  ([] (solve-1 "resources/2022/day14.txt"))
  ([file]
   (let [walls (into #{} (mapcat parse-line (h/slurp-strings file)))]
     (binding [*floating-pos* (floating-pos walls)
               *floor-pos* nil]
       (count (iterate-until-settled walls))))))

(defn solve-2
  ([] (solve-2 "resources/2022/day14.txt"))
  ([file]
   (let [walls (into #{} (mapcat parse-line (h/slurp-strings file)))]
     (binding [*floating-pos* nil
               *floor-pos* (+ 2 (floating-pos walls))]
       (count (iterate-until-settled walls))))))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    24 (solve-1 "resources/2022/day14.test.txt")
    93 (solve-2 "resources/2022/day14.test.txt")))
