(ns aoc-2023.day10
  (:require
    [clojure.test :as test :refer [deftest are]]
    [aoc.helpers :as h]))

(def sample-data 
  ["7-F7-"
   ".FJ|7"
   "SJLL7"
   "|F--J"
   "LJ.LJ"])


(def sample-2 
  [".F----7F7F7F7F-7...."
   ".|F--7||||||||FJ...."
   ".||.FJ||||||||L7...."
   "FJL7L7LJLJ||LJ.L-7.."
   "L--J.L7...LJS7F-7L7."
   "....F-J..F7FJ|L7L7L7"
   "....L7.F7||L7|.L7L7|"
   ".....|FJLJ|FJ|F7|.LJ"
   "....FJL-7.||.||||..."
   "....L---J.LJ.LJLJ..." ])

(def sample-3
  ["FF7FSF7F7F7F7F7F---7"
   "L|LJ||||||||||||F--J"
   "FL-7LJLJ||||||LJL-77"
   "F--JF--7||LJLJ7F7FJ-"
   "L---JF-JLJ.||-FJLJJ7"
   "|F|F-JF---7F7-L7L|7|"
   "|FFJF7L7F-JF7|JL---7"
   "7-L-JL7||F7|L7F-7F7|"
   "L.L7LFJ|||||FJL7||LJ"
   "L7JLJL-JLJLJL--JLJ.L"])

(def input-file "resources/2023/day10.txt")

(def ends 
  {\S #{:start, :up, :dn, :lt, :rt}
   \F #{:dn, :rt}
   \| #{:up, :dn}
   \L #{:up, :rt}
   \- #{:lt, :rt}
   \J #{:lt, :up}
   \7 #{:lt, :dn}})

(def ends-rev
  {#{:dn :rt} \F
   #{:up :dn} \|
   #{:up :rt} \L
   #{:lt :rc} \-
   #{:lt :up} \J
   #{:lt :dn} \7})

(def dir-inverse 
  {:up :dn
   :dn :up
   :lt :rt
   :rt :lt})

(defn move [xy dir]
  (condp = dir
    :lt (update xy :x dec)
    :rt (update xy :x inc)
    :up (update xy :y dec)
    :dn (update xy :y inc))) 

(defn exit-pos [m xy dir]
  (let [new-xy (move xy dir)]
    (when-let [next-direction (first (remove #(= (dir-inverse dir) %) (ends (m new-xy))))]
      [new-xy next-direction])))

(defn with-fixed-start-pos [m start-xy dir1 dir2]
  (assoc m start-xy (ends-rev #{dir1 dir2})))

(defn run-until-start 
  "returns a) set with points in the path, b) map with 'S' changed to proper pipe char"
  [m start-xy start-dir]
  (loop [pos start-xy, dir start-dir, path [[start-xy, dir]]]
    (when-let [[npos ndir] (exit-pos m pos dir)]
      (if-not (= npos start-xy)
        (recur npos ndir (conj path [pos dir]))
        ; success:
        [(set (map first (conj path [pos dir])))  ; set with only coords taken while traversing
         (with-fixed-start-pos m start-xy start-dir (dir-inverse dir))] ; adjusted map
        )) ) )

(defn start-pos [m]
  (ffirst (filter (fn [[_xy ch]] (= ch \S)) m)))

(def ^:dynamic *initial-direction* :rt)

(defn inside? [m trace {:keys [x y]}]
  (let [crosses (->> trace
                  (filterv #(= (:y %) y))
                  (filterv #(< (:x %) x))
                  (sort-by (fn [elt] [(:x elt) (:y elt)]))
                  (mapv m)
                  (filterv #{\| \L \J})
                  count)]
    (= 1 (mod crosses 2))))

(defn count-insides [m]
  (let [sxy (start-pos m)
        [trace good-map] (run-until-start m sxy *initial-direction*)
        {:keys [xmin ymin xmax ymax]} (h/map-dimensions good-map)
        coords (for [y (range ymin (inc ymax))
                     x (range xmin (inc xmax))]
                 {:x x :y y})
        coords (filterv #(nil? (trace %)) coords)
        coords (filterv #(inside? m trace %) coords)]
    (count coords)))


(defn solve-1
  ([] (solve-1 (first (h/slurp-xy-map input-file))))
  ([m] (->>
         (run-until-start m (start-pos m) *initial-direction*)
         first
         count
         (#(/ % 2)))))

(defn solve-2
  ([] (solve-2 (first (h/slurp-xy-map input-file))))
  ([m] (count-insides m)))

(deftest test-stuff [] 
  (are [x y] (= x y)
    8 (solve-1 (first (h/make-xy-map sample-data)))
    8 (binding [*initial-direction* :rt](solve-2 (first (h/make-xy-map sample-2))))))

(comment
  (solve-1)
  (solve-2))
