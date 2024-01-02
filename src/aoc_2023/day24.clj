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
      m
      )))

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

    (spit "/tmp/aoc-24.m" (str/join "\n" 
                            ["pkg load symbolic;"
                             "syms t0 t1 t2 x y z dx dy dz;"
                             (format " x0 = %d;  y0 = %d;  z0 = %d;" x0 y0 z0)
                             (format "dx0 = %d; dy0 = %d; dz0 = %d;" dx0 dy0 dz0)
                             (format " x1 = %d;  y1 = %d;  z1 = %d;" x1 y1 z1)
                             (format "dx1 = %d; dy1 = %d; dz1 = %d;" dx1 dy1 dz1)
                             (format " x2 = %d;  y2 = %d;  z2 = %d;" x2 y2 z2)
                             (format "dx2 = %d; dy2 = %d; dz2 = %d;" dx2 dy2 dz2)
                             "s = solve(..."
                             "  x0 + t0 * dx0 == x + t0 * dx, ..."
                             "  y0 + t0 * dy0 == y + t0 * dy, ..."
                             "  z0 + t0 * dz0 == z + t0 * dz, ..."
                             "  x1 + t1 * dx1 == x + t1 * dx, ..."
                             "  y1 + t1 * dy1 == y + t1 * dy, ..."
                             "  z1 + t1 * dz1 == z + t1 * dz, ..."
                             "  x2 + t2 * dx2 == x + t2 * dx, ..."
                             "  y2 + t2 * dy2 == y + t2 * dy, ..."
                             "  z2 + t2 * dz2 == z + t2 * dz, ..."
                             "  x, dx, y, dy, z, dz, t0, t1, t2);"
                             "x = s.x"
                             "y = s.y"
                             "z = s.z"
                             "dx = s.dx"
                             "dy = s.dy"
                             "dz = s.dz"]))
    (parse-eqn-octave-output (:out (sh "octave" "/tmp/aoc-24.m")))))


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
