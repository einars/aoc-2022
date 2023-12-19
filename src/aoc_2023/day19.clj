(ns aoc-2023.day19
  (:require
    [clojure.test :as test :refer [deftest are]]
    [clojure.string :as str]
    [clojure.walk :as walk]
    [clojure.core.match :refer [match]]
    [instaparse.core :as insta]
    [aoc.helpers :as h]))

(def sample-file "resources/2023/day19.sample.txt")
(def input-file "resources/2023/day19.txt")

(def wf-parser (insta/parser "
    <workflow> = name <'{'>  comparison <'}'>

    comparison = variable op number <':'> res <','> res
    <res> = next-point | comparison
    next-point = 'A' | 'R' | #'[a-z]+'

    <op> = '<' | '>'
    fin = 'A' | 'R'
    <name> = #'[\\da-z]+'
    variable = #'[a-z]+'
    number = #'[\\d]+'
    "))


(defn parse-workflow [s]
  (let [[name comps] 
        (->> (wf-parser s)
          (walk/postwalk (fn [elt] 
                           (match [elt]
                             [[:next-point z]] (keyword z)
                             [[:variable z]] (keyword z)
                             [[:number z]] (parse-long z)
                             [[:comparison a op b x1 x2]] [(resolve (symbol op)) a b x1 x2]
                             :else elt))))]
    [(keyword name) comps]))

(defn split-kv [s]
  (let [[a b] (str/split s #"=")]
    [(keyword a) (parse-long b)]))

(defn parse-input [s]
  (into {} (as-> s XX
             (str/replace XX #"[{}]" "")
             (str/split XX #",")
             (map split-kv XX)) ))

(defn parse-problem [f]
  (let [[rules inputs] (-> f slurp (str/split #"\n\n"))
        rules (str/split rules #"\n")
        rules (reduce (fn [m [k v]] (assoc m k v)) {} (map parse-workflow rules))
        inputs (mapv parse-input (str/split inputs #"\n"))]
    {:rules rules :inputs inputs}))


(defn run-problem 
  ([rules inputs] (run-problem rules inputs :in))
  ([rules inputs state]
   (match state
     nil (throw (Exception. "NILSTATE"))
     :R false
     :A true
     [cmp op1 op2 pass fail] (recur rules inputs (if (cmp (op1 inputs) op2) pass fail))
     state (run-problem rules inputs (rules state)))))

(defn solve-1
  ([] (solve-1 input-file))
  ([f] (let [{:keys [rules inputs]} (parse-problem f)]
         (->> inputs
           (filterv #(run-problem rules %))
           (map #(apply + ((juxt :x :m :a :s) %)))
           (reduce +)))))

(defn solve-2
  ([] (solve-2 (h/slurp-strings input-file)))
  ([m] (->> m
         )))

(solve-1 sample-file)

(deftest test-stuff [] 
  (are [x y] (= x y)
    [:xr [#'< :a 2738 :hnx :nxz]] (parse-workflow "xr{a<2738:hnx,nxz}" )
    [:rfg [#'< :s 537 :gd [#'> :x 2440 :R :A]]] (parse-workflow "rfg{s<537:gd,x>2440:R,A}")
    {:x 787, :m 2655, :a 1222, :s 2876} (parse-input "{x=787,m=2655,a=1222,s=2876}")
    19114 (solve-1 sample-file)
    ;0 (solve-2 sample-data)
    ))

(comment
  (solve-1)
  (solve-2))
