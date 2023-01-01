(ns aoc.helpers
  (:require
    [clojure.string :as str]
    [clojure.edn :as edn]
    [clojure.java.io :as io]))

(defn slurp-strings [file]
  (with-open [rdr (io/reader file)]
    (reduce conj [] (line-seq rdr))))

(defn slurp-blocks [file]
  (mapv #(str/split % #"\n")
    (str/split (slurp file) #"\n\n")))

(def ^:dynamic *map-ignore* #{\. \space})

(defn- index-line 
  "returns list of [coordinate character]"
  [[line y]]
  (for [[elt x] (map vector line (range)) :when (not (*map-ignore* elt))]
    [{:x x :y y} elt]))

(defn make-xy-map
  [lines]
  (let [indexed-lines (map vector lines (range))
        xy-maps (map index-line indexed-lines)
        xy-map(reduce (fn [xy elt] (into xy elt)) {} xy-maps)
        size-y (count indexed-lines)
        size-x (count (first (first indexed-lines)))]
    [xy-map {:x size-x :y size-y}]))

(defn slurp-xy-map
  "read the file as a 2d visual map and return a hashmap of coordinate -> character and its dimensions"
  [file]
  (make-xy-map (slurp-strings file)))

(defn zip [& xs] (apply map vector xs))

(defn indexed [xs]
  (let [n (count xs)]
    (map vector (range n) xs)))

(defn slurp-ints [file]
  ; use end/read-string as a general way to avoid Integer/parseInt being unable to read bigints etc
  (map edn/read-string (slurp-strings file)))

(defn find-keys [pred m]
  (map first (filter (fn [[_k v]] (pred v)) m)))

(defn find-vals [pred m]
  (map second (filter (fn [[_k v]] (pred v)) m)))

