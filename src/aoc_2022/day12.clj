(ns aoc-2022.day12
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn find-keys [pred m]
  (map first (filter (fn [[_k v]] (pred v)) m)))

(defn find-vals [pred m]
  (map second (filter (fn [[_k v]] (pred v)) m)))

(defn parse
  [ch]
  (condp = ch
    \S { :start true, :path [], :elevation 0}
    \E { :finish true, :elevation (- (int \z) (int \a))}
    {:elevation (- (int ch) (int \a)) }))

(defn parse-reverse
  [ch]
  (condp = ch
    \S { :finish true, :elevation 0}
    \E { :start true, :path [], :elevation (- (int \z) (int \a))}
    \a { :finish true, :elevation 0}
    {:elevation (- (int ch) (int \a)) }))

(defn neighbors
  [{:keys [x y]}]
  [{:x (inc x) :y y}
   {:x (dec x) :y y}
   {:x x :y (inc y)}
   {:x x :y (dec y)}])

(defn stock-elevation-fn [my-elevation look-at] (>= (inc my-elevation) look-at))
(def ^:dynamic *elevation-fn* stock-elevation-fn)

(defn consider-step 
  [m coord step]
  (let [cur-elevation (:elevation (m coord))
        nbs (->> 
              (neighbors coord)
              (filter m)
              (filter #(nil? (:path (m %))))
              (filter #(*elevation-fn* cur-elevation (:elevation (m %)))))]
    (reduce (fn [new-m c]
              (-> new-m
                (assoc-in [c :path] (conj (:path (m coord)) c))
                (assoc-in [c :step] step))) m nbs)))

(defn find-path 
  ([m] (find-path m (find-keys :start m) 0))
  ([m pool step]
   (let [new-map (reduce (fn [new-m coord] (consider-step new-m coord step)) m pool)
         new-pool (find-keys #(= step (:step %)) new-map)]
     (if (seq new-pool)
       (recur new-map new-pool (inc step))
       (map :path (find-vals #(and (:finish %) (:path %)) new-map))))))

;(count (first (find-path (update-vals (first (h/slurp-xy-map "resources/2022/day12.test.txt")) parse))))

(defn solve-1
  ([] (solve-1 "resources/2022/day12.txt"))
  ([file]
   (let [[m _dimensions] (h/slurp-xy-map file)]
     (count (first (find-path (update-vals m parse)))))))

(defn solve-2
  ([] (solve-2 "resources/2022/day12.txt"))
  ([file]
   (let [[m _dimensions] (h/slurp-xy-map file)]
    (binding [*elevation-fn* (fn [a b] (stock-elevation-fn b a))] 
      (first (sort < (map count (find-path (update-vals m parse-reverse)))))))))


(deftest test-stuff [] 
  (test/are [x y] (= x y)
    31 (solve-1 "resources/2022/day12.test.txt")
    29 (solve-2 "resources/2022/day12.test.txt")))
