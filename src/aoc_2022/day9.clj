(ns aoc-2022.day9
  (:require [clojure.test :as test :refer [deftest]])
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:require [clojure.tools.trace :refer [trace deftrace]])
  (:require [clojure.pprint :as pp])
  (:require [aoc.helpers :as h]))


; [head tail]
(def blank-rope [{:x 0 :y 0}, {:x 0 :y 0}])
(def blank-rope-pt2 (repeat 10 {:x 0 :y 0}))

(def directions 
  {\U {:dy +1}
   \D {:dy -1}
   \L {:dx -1}
   \R {:dx +1}})

(defn apply-direction [{:keys [x y]} {:keys [dx dy]}]
  {:x (+ x (or dx 0))
   :y (+ y (or dy 0))})

(defn should-tail-move? 
  [head tail]
  (or (> (abs (- (:x head) (:x tail))) 1)
    (> (abs (- (:y head) (:y tail))) 1)))

(defn adjust-tail 
  [head tail]
  (if (not (should-tail-move? head tail))
    tail
    (-> tail
      (update :x #(cond
        (= % (head :x)) %
        (> % (head :x)) (dec %)
        (< % (head :x)) (inc %)))
      (update :y #(cond
        (= % (head :y)) %
        (> % (head :y)) (dec %)
        (< % (head :y)) (inc %))))))

(defn move-rope [rope dir]
  (let [delta (directions dir)
        new-head (apply-direction (first rope) delta)]
    (reduce (fn [acc this-tail] (conj acc (adjust-tail (last acc) this-tail)))
      [new-head]
      (rest rope))))


(defn move-rope-multi 
  [rope [dir amount]]
  (loop [rope rope, amount amount, acc []]
    (if (zero? amount)
      acc
      (let [new-rope (move-rope rope dir)]
        (recur new-rope (dec amount) (conj acc new-rope))))))

(defn run-with-commands
  "returns [end-rope full-path]"
  [rope commands]
  (reduce (fn [[r, path] cmd]
            (let [new-path (move-rope-multi r cmd)]
              [(last new-path) (concat path new-path)]))
    [rope, [rope]] commands))

(defn tail-positions
  [path]
  (set (map last path)))

(defn parse-command
  [cmd]
  [(first cmd) (Integer/parseInt (subs cmd 2))])

(defn solve-1
  ([] (solve-1 "resources/2022/day9.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (map parse-command)
     (run-with-commands blank-rope)
     second
     tail-positions
     count)))

(defn solve-2
  ([] (solve-2 "resources/2022/day9.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (map parse-command)
     (run-with-commands blank-rope-pt2)
   second
   tail-positions
   count)))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    false (should-tail-move? {:x 0 :y 0} {:x 0 :y 0})
    false (should-tail-move? {:x 0 :y 0} {:x 0 :y 1})
    true (should-tail-move? {:x 0 :y 0} {:x 0 :y 2})
    [{:x 4 :y 0} {:x 3 :y 0}] (first (run-with-commands blank-rope [[\R 4]]))
    [{:x 4 :y 4} {:x 4 :y 3}] (first (run-with-commands blank-rope [[\R 4] [\U 4]]))
    [{:x 1 :y 4} {:x 2 :y 4}] (first (run-with-commands blank-rope [[\R 4] [\U 4] [\L 3]]))
    13 (solve-1 "resources/2022/day9.test.txt")
    1 (solve-2 "resources/2022/day9.test.txt")
    36 (solve-2 "resources/2022/day9.test2.txt")
    ))
