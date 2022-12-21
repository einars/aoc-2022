(ns aoc-2022.day21
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn parse-math [m] 
  (try
    (Integer/parseInt m)
    (catch Exception _
      (let [[_ a op b] (first (re-seq #"([a-z]+) ([+\-*\/]+) ([a-z]+)" m))]
        [(symbol op) (keyword a) (keyword b)]))))

(defn parse [s]
  (let [[name rest] (str/split s #": ")]
    [(keyword name) (parse-math rest)] ) )

(defn monkey-eval [what all]
  (let [x (all what)]
    (if (number? x)
      x
      (eval (list (first x)
              (monkey-eval (second x) all) 
              (monkey-eval (nth x 2) all))))))

(defn simplify [what all]
  (let [x (all what)]
    (cond
      (= what :humn) :humn
      (number? x) x
      :else (let [a (simplify (second x) all)
                  b (simplify (nth x 2) all)]
              (if (and (number? a) (number? b))
                (eval (list (first x) a b))
                [(first x) a b])))))

(defn solve-1
  ([] (solve-1 "resources/2022/day21.txt"))
  ([file]
   ( ->>
     (h/slurp-strings file)
     (map parse)
     (into {})
     (monkey-eval :root))))

(defn rev-eval 
  "solve backwards `(op a b) = n` when n is known. returns :humn"
  [expr n]
  (if (= expr :humn)
    n
    (let [[e n1 n2] expr]
      (cond
        (and (= '+ e) (number? n1)) (rev-eval n2 (- n n1))
        (and (= '+ e) (number? n2)) (rev-eval n1 (- n n2))

        (and (= '- e) (number? n1)) (rev-eval n2 (- n1 n))
        (and (= '- e) (number? n2)) (rev-eval n1 (+ n2 n))

        (and (= '* e) (number? n1)) (rev-eval n2 (/ n n1))
        (and (= '* e) (number? n2)) (rev-eval n1 (/ n n2))

        (and (= '/ e) (number? n1)) (rev-eval n2 (/ n1 n))
        (and (= '/ e) (number? n2)) (rev-eval n1 (* n n2))))))

(defn do-part-2 [all]
  (let [[_ root-lt root-rt] (all :root)
        lt (simplify root-lt all)
        rt (simplify root-rt all)]

    (if (number? lt) 
      (rev-eval rt lt)
      (rev-eval lt rt))))

(defn solve-2
  ([] (solve-2 "resources/2022/day21.txt"))
  ([file]
   ( ->>
     (h/slurp-strings file)
     (map parse)
     (into {})
     (do-part-2))))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    152 (solve-1 "resources/2022/day21.test.txt")
    301 (solve-2 "resources/2022/day21.test.txt")))
