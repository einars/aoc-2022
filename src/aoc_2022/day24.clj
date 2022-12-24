(ns aoc-2022.day24
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
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
      (dissoc {:x (- size-x 2) :y (dec size-y)})
      (dissoc {:x 1 :y 0}))))

(defn next-state [{:keys [wraps winds-n winds-w winds-e winds-s] :as board}]
  (-> board
    (assoc :winds-n (set (map #(or (wraps %) %) (map #(update % :y dec) winds-n))))
    (assoc :winds-s (set (map #(or (wraps %) %) (map #(update % :y inc) winds-s))))
    (assoc :winds-w (set (map #(or (wraps %) %) (map #(update % :x dec) winds-w))))
    (assoc :winds-e (set (map #(or (wraps %) %) (map #(update % :x inc) winds-e))))))

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
      [(and (nil? (winds-n me)) (nil? (winds-w me)) (nil? (winds-s me)) (nil? (winds-e me)) (nil? (wraps me)) me) ; wait
       (when (> (me :y) 0) (and (nil? (winds-n me-n)) (nil? (winds-w me-n)) (nil? (winds-s me-n)) (nil? (winds-e me-n)) (nil? (wraps me-n)) me-n))
       (and (nil? (winds-n me-w)) (nil? (winds-w me-w)) (nil? (winds-s me-w)) (nil? (winds-e me-w)) (nil? (wraps me-w)) me-w)
       (and (nil? (winds-n me-s)) (nil? (winds-w me-s)) (nil? (winds-s me-s)) (nil? (winds-e me-s)) (nil? (wraps me-s)) me-s)
       (and (nil? (winds-n me-e)) (nil? (winds-w me-e)) (nil? (winds-s me-e)) (nil? (winds-e me-e)) (nil? (wraps me-e)) me-e)])))

(defn solve 
  ([board start goal] (solve board [start] goal 0))
  ([board pool goal it]

   (prn :it it) 
   (flush)

   (when (seq pool)
     (let [new-board (next-state board)
           new-pool (reduce (fn [new-pool loc] (reduce conj new-pool (poss-actions loc new-board)) )
                      #{} pool) ]

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
;(solve-2 "resources/2022/day24.txt")
;(solve-1 "resources/2022/day24.txt")
;(solve-1 "resources/2022/day24.minitest.txt")

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    18 (solve-1 "resources/2022/day24.test.txt")
    54 (solve-2 "resources/2022/day24.test.txt")))
