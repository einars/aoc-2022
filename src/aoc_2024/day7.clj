(ns aoc-2024.day7
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [instaparse.core :as insta]
   [clojure.string :as str]
   [clojure.math :as math]
   [aoc.helpers :as h]))

(def input-txt "resources/2024/day7.txt")

(def sample
  "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(def parser (insta/parser "
<task>   = equation (<nl> equation)* <nl>?
equation = target <': '> numbers
numbers  = int (<' '> int)+
target   = int
int      = #'\\d+'
nl       = '\n'
"))

(defn parse-tree
  [t]
  (condp = (first t)
    :target {:target (second t)}
    :numbers {:numbers (vec (next t))}
    :equation (into {} (parse-tree (next t)))
    (mapv parse-tree t)))


(defn parse-input [t]
  (parse-tree (h/tree-parse-int (parser t))))

(def ^:dynamic *concat-supported* false)
(def ^:dynamic *concat-mode* :math)

(defn solve-eqn
  ([{:keys [target numbers]}]
   (solve-eqn target (reverse numbers) []))

  ([target [n & rest] accu]

   (if-not rest
     (when (= target n) (conj accu n))

     (or 
       (solve-eqn (- target n) rest (conj accu n "+"))

       (and *concat-supported*
         (= *concat-mode* :string)
         (str/ends-with? (str target) (str n))
         (let [st (str target)]
           (solve-eqn (or (parse-long (subs st 0 (- (count st) (count (str n))))) 0) 
             rest 
             (conj accu n "||"))))

       (and *concat-supported*
         (= *concat-mode* :math)
         (let [mask (reduce * (repeat (inc (int (math/log10 n))) 10))]
           (when (= n (rem target mask))
             (solve-eqn (quot target mask) rest (conj accu n "||")))))

       (when (= 0 (mod target n))
         (solve-eqn (quot target n) rest (conj accu n "*")))))))


(defn pt1
  [task]
  (->> task
    (mapv (fn [t] (assoc t :solution (solve-eqn t))))
    (filterv :solution)
    (mapv :target)
    (reduce +)))


(defn pt2
  [task]
  (binding [*concat-supported* true] (pt1 task)))

(binding [*concat-supported* true]
  (mapv (fn [x] [(:target x) (apply str (reverse (:solution x)))])
    (->> (parse-input (slurp input-txt))
      (mapv (fn [t] (assoc t :solution (solve-eqn t))))
      (filterv :solution)) ))


(defn solve-1
  ([] (solve-1 (slurp input-txt)))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp input-txt)))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    3749 (pt1 (parse-input sample))
    11387 (pt2 (parse-input sample))))


(comment
  (solve-1)
  (solve-2))
