(ns aoc-2023.day14
  (:require
    [clojure.test :as test :refer [deftest are]]
    [clojure.string :as str]
    [aoc.helpers :as h]))

(def sample-data (str/trim "
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...." ))

(def ^:dynamic *dims* {})

(def input-file "resources/2023/day14.txt")

(defn inside-bounds [ {:keys [x y]}]
  (let [{:keys [xmin xmax ymin ymax]} *dims*]
    (and (<= xmin x xmax ) (<= ymin y ymax))))

(defn move-stone [m coord dir]
  (let [new-coord (h/move coord dir)]
    (if (or (m new-coord) (not (inside-bounds new-coord)))
      m
      (recur
        (-> m
          (assoc new-coord (m coord))
          (dissoc coord))
        new-coord
        dir ))))

(defn fall [m dir]
  (let [stones (h/find-keys #{\O} m)]
    (reduce (fn [map stone] (move-stone map stone dir)) m stones)))

(defn fall-forever [m dir]
  (let [nm (fall m dir)]
    (if (= nm m)
      m
      (recur nm dir))))

(defn spin-cycle [m]
  (print ".")
  (flush)
  (-> m
    (fall-forever :up)
    (fall-forever :lt)
    (fall-forever :dn)
    (fall-forever :rt)))


(defn calc-spin [m times]
  (let [all-ms (iterate spin-cycle m)]
    (loop [[m & ms] all-ms, n 0, seen {}]
      (if-let [cycle-start (seen m)]
        (do
          (prn :cycle-found cycle-start n :len (- n cycle-start))
          (nth all-ms (+ n (mod (- times cycle-start) (- n cycle-start)))))
        (recur ms (inc n) (assoc seen m n))))))

(defn score [m]
  (let [{:keys [ymax]} (h/map-dimensions m)
        stones (h/find-keys #{\O} m)]
    (reduce + (map #(- ymax % -1) (map :y stones)))))

(defn solve-1
  ([] (solve-1 (h/slurp-map input-file)))
  ([m] (binding [*dims* (h/map-dimensions m)] 
         (score (fall-forever m :up)))))

(defn solve-2
  ([] (solve-2 (h/slurp-map input-file)))
  ([m] (binding [*dims* (h/map-dimensions m)] 
         (score (calc-spin m 1000000000)))))

;(def m (h/string->map sample-data))
;(h/print-map m)
;(h/print-map (spin-cycle m))
;(h/print-map (spin-cycle (spin-cycle m)))
;(h/print-map (spin-cycle (spin-cycle (spin-cycle m))))
;(calc-spin m 0)
;(h/print-map (fall m :lt))
;(h/print-map (fall m :up))
;(h/print-map (fall (fall m :up) :up))


(deftest test-stuff [] 
  (are [x y] (= x y)
    136 (solve-1 (h/string->map sample-data))
    64 (solve-2 (h/string->map sample-data))))


(comment
  (solve-1)
  (solve-2))
