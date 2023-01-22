(ns aoc-2020.day18
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(def eq-precedences 
  {'+ 0
   '- 0
   '* 0
   '/ 0})

(def adv-precedences 
  {'+ 5
   '- 0
   '* 0
   '/ 0})


(def ^:dynamic *precedences* eq-precedences)

(defn transform-infix 
  "shunting yard, https://codereview.stackexchange.com/questions/216123/parsing-infix-expressions-in-clojure"
  [s]
  (if (list? s)
    (loop [vals (), ops (), expr s, next-op false]
      (if next-op
        (let [precedence (if (empty? expr) -1 (*precedences* (first expr)))
              popped (take-while #(>= (*precedences* %) precedence) ops)
              res (reduce
                    (fn [[a b & vs] op]
                      (cons (list op b a) vs))
                    vals
                    popped)]
          (if (empty? expr)
            (first res)
            (recur res
              (cons (first expr) (drop (count popped) ops))
              (rest expr)
              false)))
        (recur (cons (transform-infix (first expr)) vals)
          ops
          (rest expr)
          true)))
    s))

(defn parse [s]
  ; we're wild cowboys doing read and eval on the source in the input.txt
  (transform-infix (read-string (format "(%s)" s))))


(defn solve-1
  ([] (solve-1 "resources/2020/day18.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (map parse)
     (map eval)
     (reduce +))))

(defn solve-2 [& s]
  (binding [*precedences* adv-precedences] (apply solve-1 s)))


(deftest test-stuff [] 
  (test/are [x y] (= x y)
    51 (eval (transform-infix '(1 + (2 * 3) + (4 * (5 + 6)))))
    13632 (solve-1 "resources/2020/day18.test.txt")
    23340 (solve-2 "resources/2020/day18.test.txt")))
  
