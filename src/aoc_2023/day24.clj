(ns aoc-2023.day24
  (:require
    [clojure.test :as test :refer [deftest are]]
    [clojure.java.shell :refer [sh]]
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


(defn read-octave-sym [m s]
  (let [[v val] (str/split s #" = \(sym\) ")]
    (if val
      (assoc m (keyword v) (parse-long val))
      m)))

(defn parse-eqn-octave-output [s]
  (prn s)
  (reduce read-octave-sym {} (str/split s #"\n")))

(defn solve-eqn 
  "
Finds ptA, dptA, a solution for 
 /
 | pt0 + t0 * dpt0 = ptA + t0 * dptA
<  pt1 + t1 * dpt1 = ptA + t1 * dptA
 | pt2 + t2 * dpt2 = ptA + t2 * dptA
 \\

 Outsource solving to octave.

  "
  [pt0 pt1 pt2]

  (let [{x0 :x, y0 :y, z0 :z, dx0 :dx, dy0 :dy, dz0 :dz} pt0
        {x1 :x, y1 :y, z1 :z, dx1 :dx, dy1 :dy, dz1 :dz} pt1 
        {x2 :x, y2 :y, z2 :z, dx2 :dx, dy2 :dy, dz2 :dz} pt2 ]
    (-> (sh "octave" 
          :in (str/join "\n"
                ["pkg load symbolic;"
                 "syms t0 t1 t2 x y z dx dy dz;"
                 "s = solve("
                 (format "  %d + t0 * %d == x + t0 * dx," x0 dx0)
                 (format "  %d + t0 * %d == y + t0 * dy," y0 dy0)
                 (format "  %d + t0 * %d == z + t0 * dz," z0 dz0)
                 (format "  %d + t1 * %d == x + t1 * dx," x1 dx1)
                 (format "  %d + t1 * %d == y + t1 * dy," y1 dy1)
                 (format "  %d + t1 * %d == z + t1 * dz," z1 dz1)
                 (format "  %d + t2 * %d == x + t2 * dx," x2 dx2)
                 (format "  %d + t2 * %d == y + t2 * dy," y2 dy2)
                 (format "  %d + t2 * %d == z + t2 * dz," z2 dz2)
                 "  x, dx, y, dy, z, dz, t0, t1, t2);"
                 "x = s.x, y = s.y, z = s.z"
                 "dx = s.dx, dy = s.dy, dz = s.dz"]))
      :out
      parse-eqn-octave-output)))


(defn print-solution-2 [{x :x y :y z :z :as s}]
  (prn :stone s)
  (+ x y z))


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
         (take 3)
         (mapv parse-hailstone)
         (apply solve-eqn)
         print-solution-2)))


(deftest test-stuff [] 
  (are [x y] (= x y)

    ;[14.333 15.333] (intersect-xy
    ;                  (Hailstone. 19 13 30, -2 1 -2)
    ;                  (Hailstone. 18 19 22, -1 -1 -2))
    2 (find-crashes
        (mapv parse-hailstone (str/split sample-data #"\n"))
        [7 27])
    {:a -1, :b 13} (parse-eqn-octave-output "xxx\na = (sym) -1\nb = (sym) 13\n")
    47 (solve-2 (str/split sample-data #"\n"))
    ))

(comment
  (solve-1)
  (solve-2))
