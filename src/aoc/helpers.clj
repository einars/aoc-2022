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

(defn map-dimensions [m] 
  {:xmin (apply min (map :x (keys m)))
   :xmax (apply max (map :x (keys m)))
   :ymin (apply min (map :y (keys m)))
   :ymax (apply max (map :y (keys m)))})

(defn print-map [m]
  (let [{:keys [xmin ymin xmax ymax]} (map-dimensions m)]
    (println (format "x=%d..%d, y=%d..%d" xmin xmax ymin ymax))
    (doseq [y (range ymin (inc ymax))]
      (println (apply str (for [x (range xmin (inc xmax))]
                            (get m {:x x :y y} ".")))))))
(defn repr-map [m]
  (let [{:keys [xmin ymin xmax ymax]} (map-dimensions m)]
    (apply str (for [y (range ymin (inc ymax))]
                 (str (apply str (for [x (range xmin (inc xmax))]
                                   (get m {:x x :y y} "."))) "\n")))))

(defn slurp-xy-map
  "read the file as a 2d visual map and return [hashmap of coord -> char, dimensions]
   0 1 2 3 4 5 -> x
    1
    2
    3
    |
    y
  "
  [file]
  (make-xy-map (slurp-strings file)))

(defn string->map [f]
  (-> f (str/split #"\n") make-xy-map first))

(defn slurp-map 
  "slurp-xy-map returns map and dimensions. i generally never need dimensions"
  [f]
  (-> f slurp string->map))


(defn zip [& xs] (apply map vector xs))

(defn indexed [xs]
    (map vector (range) xs))

(defn indexed-1 [xs]
  (let [n (count xs)]
    (map vector (range 1 (inc n)) xs)))

(defn slurp-ints [file]
  ; use end/read-string as a general way to avoid Integer/parseInt being unable to read bigints etc
  (map edn/read-string (slurp-strings file)))

(defn find-keys [pred m]
  (mapv first (filterv (fn [[_k v]] (pred v)) m)))

(defn find-vals [pred m]
  (mapv second (filter (fn [[_k v]] (pred v)) m)))

(defn to-int-list [s]
  (mapv #(Integer/parseInt %) (str/split (str/trim s) #",")))

(defn neighbors-h
  [{:keys [x y]}]
  [{:x (inc x) :y y}
   {:x (dec x) :y y}])

(defn neighbors-v
  [{:keys [x y]}]
  [{:x x :y (inc y)}
   {:x x :y (dec y)}])

(defn neighbors-4
  [{:keys [x y]}]
  [{:x (inc x) :y y}
   {:x (dec x) :y y}
   {:x x :y (inc y)}
   {:x x :y (dec y)}])

(defn neighbors-8
  [{:keys [x y]}]

  [{:x (dec x) :y (dec y)}
   {:x x :y (dec y)}
   {:x (inc x) :y (dec y)}

   {:x (dec x) :y y}
   ; {:x x :y y}
   {:x (inc x) :y y}

   {:x (dec x) :y (inc y)}
   {:x x :y (inc y)}
   {:x (inc x) :y (inc y)}])


(defn left-of [c] (update c :x dec))
(defn right-of [c] (update c :x inc))
(defn top-of [c] (update c :y dec))
(defn bottom-of [c] (update c :y inc))

(def moves 
  {:lt left-of
   :rt right-of
   :up top-of
   :dn bottom-of})

(defn move [c dir]
  ((get moves dir identity) c))

(defn tree-parse-int
  ; replace [:int "5"] with 5,
  ; useful for results of insta/parse
  [tree]
  (cond
    (and
      (coll? tree)
      (= :int (first tree))) (parse-long (second tree))
    (coll? tree) (mapv tree-parse-int tree)
    :else tree))

(defn count-where 
  [pred coll] 
  (reduce (fn [accu n] (if (pred n) (inc accu) accu)) 0 coll))
  
