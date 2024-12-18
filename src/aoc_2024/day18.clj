(ns aoc-2024.day18
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [clojure.tools.trace :refer :all]
   [instaparse.core :as insta]
   [aoc.helpers :as h]))

(def input-txt "resources/2024/day18.txt")

(defn as-xy [s]
  (let [[x y ] (map parse-long (str/split s #","))]
    {:x x, :y y}))

(defn parse-input
  [s]
  (let [s (str/trim s)
        ss (str/split s #"\n")]
    (mapv as-xy ss)))

(defn find-shortest-path
  [pos target walls dim]

  (loop [[x & xs :as front] #{{:pos pos :score 0}}, new-front #{}, best-scores {} ]
    (if (empty? front)
      (if (empty? new-front)
        best-scores
        (recur new-front #{} best-scores))

      (let [{:keys [pos score]} x]

        (if (< (get best-scores pos 9999999999) score)
          (recur xs new-front best-scores)

          (let [candidates (mapv (partial h/move pos) [:up :dn :lt :rt])
                candidates (filterv (complement walls) candidates)
                candidates (filterv (fn [{x :x}] (<= 0 x dim)) candidates)
                candidates (filterv (fn [{y :y}] (<= 0 y dim)) candidates)
                candidates (mapv (fn [pos] {:pos pos :score (inc score)}) candidates)
                new-front (reduce conj new-front candidates)]
            (recur xs new-front (assoc best-scores pos score))))))))


(defn solve-with [path len dim]
  (let [stones (set (take len path))]
    (get (find-shortest-path {:x 0, :y 0} {:x dim, :y dim}, stones dim)
      {:x dim :y dim } )))

(defn pt1
  [task]
  (solve-with task 1024 70))

(defn pt2
  [task]
  (loop [n 1024] 
    (prn :n n)
    (if (solve-with task n 70)
      (recur (inc n))
      (nth task (dec n)))))

(defn solve-1
  ([] (solve-1 (slurp input-txt)))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp input-txt)))
  ([ss] (pt2 (parse-input ss))))

(comment
  (solve-1)
  (solve-2))
