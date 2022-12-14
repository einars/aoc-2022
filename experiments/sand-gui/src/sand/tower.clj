(ns sand.tower
  (:require
    [clojure.string :as str]))

(def ^:dynamic *floating-pos* nil)
(def ^:dynamic *floor-pos* nil)

(defn- make-pair [s]
  (map #(Integer/parseInt %) (str/split s #",")))

(defn- make-range [x y]
  (cond
    (= x y) [x]
    (> x y) (range x (dec y) -1)
    (< x y) (range x (inc y) 1)))

(defn- trace-line
  [pts next-pt]
  (if (empty? pts) 
    (conj pts next-pt)
    (let [last-pt (last pts)]
      (concat pts (for [x (make-range (first last-pt) (first next-pt))
                        y (make-range (second last-pt) (second next-pt))]
                    [x y])))))

(defn- parse-line [s]
  (reduce #(trace-line %1 (make-pair %2)) [] (str/split s #" -> ")))

(defn parse-file
  "returns set of wall coordinates"
  [file]
  (into #{} (mapcat parse-line (str/split (slurp file) #"\n"))))

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

(defn iterate-1 [{:keys [walls sand grain] :as tower}]
  (let [grain (or grain [500 0])]
   (try 
    (if-let [new-grain (next-move grain walls sand)]
      (assoc tower :grain new-grain)
      (-> tower (assoc :sand (conj sand grain)) (assoc :grain nil)))
    (catch Exception _ (assoc tower :grain nil)))))

(defn iterate-until-settled-1 [{:keys [walls sand] :as tower} ]
  (if-let [new-sand (try (settle walls sand [500 0]) (catch Exception _ nil))]
    (assoc tower :sand (or new-sand sand))
    tower))
