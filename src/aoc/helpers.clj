(ns aoc.helpers
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io]))

(defn slurp-strings [file]
  (with-open [rdr (io/reader file)]
    (reduce conj [] (line-seq rdr))))

(defn slurp-blocks [file]
  (mapv #(str/split % #"\n")
    (str/split (slurp file) #"\n\n")))

(defn- index-line 
  "returns list of [coordinate character]"
  [[line y]]
  (for [[elt x] (map vector line (range)) :when (and (not= elt \.) (not= elt \space))]
    [{:x x :y y} elt]))

(defn slurp-xy-map
  "read the file as a 2d visual map and return a hashmap of coordinate -> character and its dimensions"
  [file]
  (let [indexed-lines (map vector (slurp-strings file) (range))
        xy-maps (map index-line indexed-lines)
        xy-map(reduce (fn [xy elt] (into xy elt)) {} xy-maps)
        size-x (count indexed-lines)
        size-y (count (first (first indexed-lines)))]
        [xy-map {:x size-x :y size-y}]))



(defn slurp-ints [file]
  (mapv #(Integer/parseInt %) (slurp-strings file)))

