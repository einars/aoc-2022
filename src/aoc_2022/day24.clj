(ns aoc-2022.day24
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    ;[flames.core :as flames]
    [aoc.helpers :as h]))

(defn make-wraps [dimensions]
  (let [size-x (:x dimensions)
        size-y (:y dimensions)
        pairs (for [x (range 0 size-x)
                    y (range 0 size-y)]
                [[{:x 0, :y y} {:x (- size-x 2) :y y}]
                 [{:x (dec size-x), :y y} {:x 1 :y y}]
                 [{:x x, :y 0} {:x x :y (- size-y 2)}]
                 [{:x x, :y (dec size-y)} {:x x :y 1}]])]
    (->
      (reduce into {} pairs)
      (assoc {:x (- size-x 2) :y size-y} :prevent-walking-out )
      (assoc {:x 1 :y -1} :prevent-walking-out )
      (dissoc {:x (- size-x 2) :y (dec size-y)})
      (dissoc {:x 1 :y 0}))))

(defn next-state [{:keys [wraps winds-n winds-w winds-e winds-s] :as board}]
  (-> board
    (assoc :winds-n (set (map #(wraps % %) (map #(update % :y dec) winds-n))))
    (assoc :winds-s (set (map #(wraps % %) (map #(update % :y inc) winds-s))))
    (assoc :winds-w (set (map #(wraps % %) (map #(update % :x dec) winds-w))))
    (assoc :winds-e (set (map #(wraps % %) (map #(update % :x inc) winds-e))))))

(defn make-map 
  [[board dimensions]]
  (prn dimensions)
  (let [wraps (make-wraps dimensions)]
    {:winds-n (set (h/find-keys #(= \^ %) board))
     :winds-w (set (h/find-keys #(= \< %) board))
     :winds-e (set (h/find-keys #(= \> %) board))
     :winds-s (set (h/find-keys #(= \v %) board))
     :start {:x 1, :y 0}
     :target {:x (- (dimensions :x) 2), :y (dec (dimensions :y))}
     :wraps wraps}))


(defn poss-actions [me {:keys [winds-n winds-w winds-e winds-s wraps]}]
  (let [me-n (update me :y dec)
        me-s (update me :y inc)
        me-w (update me :x dec)
        me-e (update me :x inc)]

    (filter identity
      [(when-not (or (wraps me) (winds-n me) (winds-w me) (winds-s me) (winds-e me)) me) ; wait
       (when-not (or (wraps me-n) (winds-n me-n) (winds-w me-n) (winds-s me-n) (winds-e me-n)) me-n)
       (when-not (or (wraps me-w) (winds-n me-w) (winds-w me-w) (winds-s me-w) (winds-e me-w)) me-w)
       (when-not (or (wraps me-s) (winds-n me-s) (winds-w me-s) (winds-s me-s) (winds-e me-s)) me-s)
       (when-not (or (wraps me-e) (winds-n me-e) (winds-w me-e) (winds-s me-e) (winds-e me-e)) me-e)])))

(defn solve 
  ([board start goal] (solve board [start] goal 0))
  ([board pool goal it]

   (prn :it it :n (count pool)) 
   (flush)

   (when (seq pool)
     (let [new-board (next-state board)
           new-pool (reduce (fn [new-pool loc] (reduce conj new-pool (poss-actions loc new-board)) )
                      #{} pool)]

       (if (new-pool goal)
         [new-board (inc it)]
         (recur new-board new-pool goal (inc it)))))))

(defn solve-1
  ([] (solve-1 "resources/2022/day24.txt"))
  ([file]
   (let [b (make-map (h/slurp-xy-map file))]
     (second (solve b (:start b) (:target b))))))

(defn solve-2
  ([] (solve-2 "resources/2022/day24.txt"))
  ([file]
   (let [b0 (make-map (h/slurp-xy-map file))
         [b1 c1] (solve b0 (:start b0) (:target b0))
         [b2 c2] (solve b1 (:target b0) (:start b0))
         [_b3 c3] (solve b2 (:start b0) (:target b0))]
     (+ c1 c2 c3))))


;(solve-1 "resources/2022/day24.test.txt")
;(solve-2 "resources/2022/day24.test.txt")
;(solve-1 "resources/2022/day24.txt")
;(solve-2 "resources/2022/day24.txt")
;(solve-1 "resources/2022/day24.minitest.txt")

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    18 (solve-1 "resources/2022/day24.test.txt")
    54 (solve-2 "resources/2022/day24.test.txt")))

