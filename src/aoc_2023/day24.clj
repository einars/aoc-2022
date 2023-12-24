(ns aoc-2023.day24
  (:require
    [clojure.test :as test :refer [deftest are]]
    [clojure.string :as str]
    [clojure.walk :as walk]
    [clojure.core.match :refer [match]]
    [instaparse.core :as insta]
    [aoc.helpers :as h]))

(defrecord Hailstone [x y z dx dy dz])

(def sample-data (str/trim
                   "
19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
  "))

(def hs-parser (insta/parser "
    hailstone = nnn <ws> <'@'> <ws>  nnn
    nnn = n <','> <ws> n <','> <ws> n
    ws = #'[ \n\r\t]+'
    n = #'-?[\\d]+'
    "))


(defn parse-hailstone [s]
  (let [e (->> (hs-parser s)
            (walk/postwalk (fn [elt] 
                             (match [elt]
                               [[:n z]] (parse-long z)
                               [[:nnn a b c]] [a b c]
                               [[:hailstone [x y z] [dx dy dz]]]
                               (Hailstone. x y z dx dy dz)
                               :else elt))))]
    (when-not e (throw (Exception. (format "Unable to parse: %s" e))))
    e ))

(parse-hailstone "19, 13, 30 @ -2,  1, -2")

(defn intersect-xy [ha hb]

  (let [x1 (:x ha)
        y1 (:y ha)
        x2 (+ (:x ha) (:dx ha))
        y2 (+ (:y ha) (:dy ha))
        x3 (:x hb)
        y3 (:y hb)
        x4 (+ (:x hb) (:dx hb))
        y4 (+ (:y hb) (:dy hb))

        div (- (* (- x1 x2) (- y3 y4))
              (* (- y1 y2) (- x3 x4)))]

    (if (= 0 div)
      nil

      (let [t (/ (- (* (- x1 x3) (- y3 y4))
                   (* (- y1 y3) (- x3 x4))) div)
            u (/ (- (* (- x1 x3) (- y1 y2))
                   (* (- y1 y3) (- x1 x2))) div)
            ]
        (when (and (>= t 0) (>= u 0))
          [(double (+ x1 (* t (:dx ha))))
           (double (+ y1 (* t (:dy ha))))])))))



(def input-file "resources/2023/day24.txt")

(defn find-crashes [hss [f t]]
  (let [res (atom 0)]
    (doseq [a hss]
      (doseq [b hss :when (not= a b)]
        (when-let [[ix iy] (intersect-xy a b)]
          (when (and (<= f ix t)
                  (<= f iy t))
            (swap! res inc) ) )) ) 

    (/ @res 2)))

(defn solve-1
  ([] (solve-1 (h/slurp-strings input-file)))
  ([m] (-> (mapv parse-hailstone m)
         (find-crashes [200000000000000 400000000000000] ))))

(defn solve-2
  ([] (solve-2 (h/slurp-strings input-file)))
  ([m] (->> m
         )))


(deftest test-stuff [] 
  (are [x y] (= x y)

    ;[14.333 15.333] (intersect-xy
    ;                  (Hailstone. 19 13 30, -2 1 -2)
    ;                  (Hailstone. 18 19 22, -1 -1 -2))
    2 (find-crashes
        (mapv parse-hailstone (str/split sample-data #"\n"))
        [7 27])

    ;0 (solve-1 sample-data)
    ;0 (solve-2 sample-data)
    ))

(comment
  (solve-1)
  (solve-2))
