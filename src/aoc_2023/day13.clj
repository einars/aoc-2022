(ns aoc-2023.day13
  (:require
    [clojure.test :as test :refer [deftest are]]
    [clojure.string :as str]
    [clojure.pprint :as pprint]
    [aoc.helpers :as h]))

(def sample-input "resources/2023/day13.sample.txt")
(def input-file "resources/2023/day13.txt")

(defn is-mirror? [elts n]
  (let [[a b] (split-at n elts)
        len (min (count a) (count b))
        a (take len (reverse a))
        b (take len b)]
    (when (= a b) n)))

(defn find-mirror [elts]
  (some
    #(is-mirror? elts %)
    (range 1 (count elts))))

(defn setize-map [m]
  (let [{:keys [xmin xmax ymin ymax]} (h/map-dimensions m)]

    {:cols (for [x (range xmin (inc xmax))]
             (->> (keys m)
               (filter #(= x (:x %)))
               (mapv :y)
               set))

     :rows (for [y (range ymin (inc ymax))]
             (->> (keys m)
               (filter #(= y (:y %)))
               (mapv :x)
               set))

     :m m }))

(defn do-solve-1 [m]
  (if-let [a (find-mirror (:cols m))]
    a
    (if-let [b (find-mirror (:rows m))]
      (* 100 b)
      (do
        (prn :no-solution)
        (h/print-map (:m m))
        0 ))))

(defn read-map [s]
  (setize-map (first (h/make-xy-map (str/split s #"\n")))))

(defn read-maps [s]
  (mapv read-map (str/split (slurp s) #"\n\n")))

(defn solve-1
  ([] (solve-1 (read-maps input-file)))
  ([m] (->> m
         (map do-solve-1)
         (reduce +))))

(defn solve-2
  ([] (solve-2 (h/slurp-strings input-file)))
  ([m] (->> m
         )))

;(find-mirror (:cols (first (read-maps sample-input))))

(deftest test-stuff [] 
  (are [x y] (= x y)
    405 (solve-1 (read-maps sample-input))
    0 (solve-2 sample-input)))

(comment
  (solve-1)
  (solve-2))
