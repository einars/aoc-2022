(ns aoc-2024.day13
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [clojure.tools.trace :refer :all]
   [instaparse.core :as insta]
   [aoc.helpers :as h]))

(def input-txt "resources/2024/day13.txt")

(def ^:dynamic *part* 1)

(def sample
  "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
")

(def parser (insta/parser
              "
<problems> = machine (<nl> machine)* <nl>*
machine = button_a <nl> button_b <nl> prize <nl>
button_a = <'Button A: X+'> int <', Y+'> int
button_b = <'Button B: X+'> int <', Y+'> int
prize = <'Prize: X='> int <', Y='> int
int   = #'\\d+'
nl    = '\n'
  "))

(defn prettier-machine [[_ [_ ax ay] [_ bx by] [_ x y]]]
  (if (= *part* 1)
    [ax ay bx by x y]
    [ax ay bx by (+ 10000000000000 x) (+ 10000000000000 y)]
    )
  )


(defn find-loop [ax ay bx by x y]

  (loop [n 0, seen-mods {}, seen-zero? false]

    (let [mod-x (mod (- x (* n ax)) bx)
          mod-y (mod (- y (* n ay)) by)
          last-seen (seen-mods [mod-x mod-y])]
      (cond
        (and seen-zero? last-seen (= 0 mod-x mod-y)) ; zero loop
        [last-seen (- n last-seen)]

        (and last-seen (not seen-zero?)) ; bad loop
        nil

        :else
        (recur (inc n) (assoc seen-mods [mod-x mod-y] n) (or seen-zero? (= 0 mod-x mod-y)))) ) ) ) 

(defn solve-one [[ax ay bx by x y]]
  (when-let [[ls ll] (find-loop ax ay bx by x y)]
    (loop [n ls, best-solution nil]

      (let [times-a n
            times-b (quot (- x (* n ax)) bx)
            check-x (+ (* times-a ax) (* times-b bx))
            check-y (+ (* times-a ay) (* times-b by))
            score (+ (* times-a 3) (* times-b 1))

            solution {:times-a times-a
                      :times-b times-b
                      :score score }]
        (cond
          (and (= *part* 1) (> times-a 100)) best-solution
          (and (= *part* 1) (> times-b 100)) (recur (+ n ll) best-solution)

          (or (< times-a 0) (< times-b 0)) best-solution

          (not= check-y y) (recur (+ n ll) best-solution)

          (nil? best-solution) (recur (+ n ll) solution)

          (< (:score solution) (:score best-solution)) (recur (+ n ll) solution)

          ;:else (recur (+ n ll) best-solution)
          :else best-solution ; won't get any better
          )))))

(defn solve-eqn [[ax ay bx by x y]]
  (let [ma (- (* ax y) (* ay x))
        mb (- (* ax by) (* ay bx))
        m (quot ma mb)]
    (when (= 0 (rem ma mb))
      (let [na (- x (* m bx))
            nb ax
            n (quot na nb) ]
        (when (= 0 (rem na nb))
          [n m] )))))

(defn parse-input
  [s]
  (map prettier-machine (h/tree-parse-int (parser s))))

(defn pt1
  [task]
  (reduce + (mapv #(:score (solve-one %) 0) task) ))


(defn pt2
  [task]
  (binding [*part* 2]
    (reduce + (map (fn [[a b]] (+ b (* a 3))) (filter some? (map solve-eqn task))))))


(pt2 (parse-input (slurp input-txt)))

(defn solve-1
  ([] (solve-1 (slurp input-txt)))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp input-txt)))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    480 (pt1 (parse-input sample))))

(pt2 (parse-input sample))
(comment
  (solve-1)
  (solve-2))
