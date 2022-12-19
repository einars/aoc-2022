(ns aoc-2022.day19
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(defn ore-cost [xs blueprints]
  (reduce + 
    (map (fn [[what n]] (if (= :ore what)
                          n 
                          (* n (ore-cost (blueprints what) blueprints))))
      xs)))

(defn calc-ore-costs [blueprints]
  (assoc blueprints :cost 
    {:geode (ore-cost (blueprints :geode []) blueprints)
     :clay (ore-cost (blueprints :clay []) blueprints)
     :obsidian (ore-cost (blueprints :obsidian []) blueprints)
     :ore (ore-cost (blueprints :ore []) blueprints)}))

(defn parse-robot-costs
  [s]
  (for [[_ res-count res-type] (re-seq #"(\d+) ([a-z]+)" s)]
    [(keyword res-type) (Integer/parseInt res-count)]))

(defn parse-robot-defs 
  "Returns [:type { :prerequisite-type count }]"
  [s]
  (for [[_ robot-type robot-costs] (re-seq #"Each ([a-z]+) robot costs ([^.]+)\." s)]
    [(keyword robot-type) (into {} (parse-robot-costs robot-costs))]))

(defn parse-blueprint
  [s]
  ;  (let [[_ bp-id costs] (re-seq #"Blueprint (\d+): (Each ([a-z]+) robot costs ((\d+) ([a-z]+)( and )?)\.+)" s)]
  (let [[_ bp-id robot-defs] (first (re-seq #"^Blueprint (\d+): (.*)" s))] 
    [(Integer/parseInt bp-id) (calc-ore-costs (into {} (parse-robot-defs robot-defs)))]))

(defn gather-resources
  [{:keys [robots resources]}]

  {:robots robots
   :resources (reduce (fn [resources [res n]] (assoc resources res (+ n (resources res 0)))) resources robots)})
  

(defn build-robot
  [{:keys [resources robots]} robot blueprints]
  (let [new-robots (assoc robots robot (inc (robots robot 0)))
        new-resources (reduce (fn [resources [res n]] (update resources res #(- % n))) resources (blueprints robot))]
    {:robots new-robots :resources new-resources}))

(defn can-build?
  [blueprint resources]
  (not (some (fn [[res n]] (< (resources res 0) n)) blueprint)))

(defn apply-action
  "Returns {:robots new-robots :resources new-resources}"
  [{:keys [act robot]} state blueprints]
  (condp = act
    :do-nothing (gather-resources state)
    :build      (build-robot (gather-resources state) robot blueprints)))



(defn possible-actions [{:keys [resources] :as state} blueprints]
  (let [possible-robots    (for [robot (keys blueprints) :when (can-build? (blueprints robot) resources)]
                             [{:act :build :robot robot} state])]
    (if (empty? possible-robots) 
      [[{:act :do-nothing} state]]
      (conj possible-robots [{:act :do-nothing} state]))))

(def initial-resources {:ore 1})

(defn score [{:keys [robots]}]
  (+ (:geode robots 0))
  (:obsidian robots 0)
  (:clay robots 0))

(defn compact-pool-2 [pool]
  (take 10000 (reverse (sort-by (fn [e]
                                  [(:geode (:resources e))
                                   (:geode (:robots e))
                                   (:obsidian (:robots e)) 
                                   (:clay (:robots e)) 
                                   (:ore (:robots e))]) (set pool)))))

(defn full-search
  [blueprints day pool]

  (printf "%d. %d\n" day (count pool))
  (flush)

  (if (= day 0)
    (do
      (printf "Best: %d\n" (:geode (:resources (first pool)) 0))
      (:geode (:resources (first pool)) 0) )

    (let [new-pool (->> pool
                     (mapcat #(possible-actions % blueprints))
                     (map (fn [[act state]] (apply-action act state blueprints))))]
      (recur blueprints (dec day) (compact-pool-2 new-pool)))))



(defn best-geodes-f ([blueprints days-left] 
                     (full-search blueprints days-left [{:resources {} :robots {:ore 1}}])))

(comment
  (best-geodes-f (second (first test-geodes)) 24)
  (best-geodes-f (second (nth prod-geodes 28)) 24)
  (best-geodes-f (second (first test-geodes)) 32))


(defn get-quality-level [[bp-id blueprints]]
  (prn bp-id)
  (* bp-id (best-geodes-f blueprints 24)))

(defn get-best-pt2 [[bp-id blueprints]]
  (prn bp-id)
  (best-geodes-f blueprints 32))

(defn solve-1
  ([] (solve-1 "resources/2022/day19.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (map parse-blueprint)
     (map get-quality-level)
     (reduce +))))

(defn solve-2
  ([] (solve-2 "resources/2022/day19.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (take 3)
     (map parse-blueprint)
     (map get-best-pt2)
     (reduce *))))

(def test-geodes (map parse-blueprint (h/slurp-strings "resources/2022/day19.test.txt")))
(def prod-geodes (map parse-blueprint (h/slurp-strings "resources/2022/day19.txt")))

;(solve-2)

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    9 (best-geodes-f (second (first test-geodes)) 24)
    12 (best-geodes-f (second (second test-geodes)) 24)
    33 (solve-1 "resources/2022/day19.test.txt")))
