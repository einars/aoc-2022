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
     :ore (ore-cost (blueprints :ore []) blueprints)
     }))


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

(defn best-geode [many-resources] (apply max many-resources))

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

(defn compact-pool [pool]
  (printf "comp %d\n" (count pool))
  (flush)
  (let [new-pool (set (filter
                        (fn [{:keys [resources robots] :as me}]
                          (not (some 
                                 ; find somebody who has more of my resources?
                                 (fn [other]
                                   (and
                                     (>= (:clay (:resources other) 0) (:clay resources 0))
                                     (>= (:ore (:resources other) 0) (:ore resources 0))
                                     (>= (:obsidian (:resources other) 0) (:obsidian resources 0))
                                     (>= (:clay (:robots other) 0) (:clay robots 0))
                                     (>= (:ore (:robots other) 0) (:ore robots 0))
                                     (>= (:obsidian (:robots other) 0) (:obsidian robots 0))
                                     (not= me other))) pool)))
                        pool))]
    (printf "-> %d\n" (count new-pool))
    new-pool
    ))


(defn compact-pool-2 [pool]
  (take 10000 (reverse (sort-by (fn [e]
                                  [(:geode (:resources e))
                                   (:geode (:robots e))
                                   (:obsidian (:robots e)) 
                                   (:clay (:robots e)) 
                                   (:ore (:robots e))]) (set pool)))))

(defn find-good-geode-builds
  [blueprints day pool n-geodes]

  (printf "%d. %d, looking for g-%d\n" day (count pool) n-geodes)
  (flush)

  (if (= day 0)
    (do
      (printf "Best: %d\n" (:geode (:resources (first pool)) 0))
      (:geode (:resources (first pool)) 0) )

    (let [magic-pool (filter #(= n-geodes (get-in % [:robots :geode])) pool)]

      (if (seq magic-pool)
        (do
          (printf "FOUND A GEODE ON DAY %d, %d ways, YAIIIII\n" day (count magic-pool))
          (find-good-geode-builds blueprints day magic-pool (inc n-geodes)))
        (let [new-pool (->> pool
                         (mapcat #(possible-actions % blueprints))
                         (map (fn [[act state]] (apply-action act state blueprints))))]
          (recur blueprints (dec day) (compact-pool-2 new-pool) n-geodes))))))

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



(defn best-geodes ([blueprints days-left] 
                   (find-good-geode-builds blueprints days-left [{:resources {} :robots {:ore 1}}] 1)))
;(full-search blueprints days-left [{:resources {} :robots {:ore 1}}])))

(defn best-geodes-f ([blueprints days-left] 
                     (full-search blueprints days-left [{:resources {} :robots {:ore 1}}])))

(comment
  (best-geodes (second (first test-geodes)) 24)
  (best-geodes (second (second test-geodes)) 24)
  (best-geodes (second (nth prod-geodes 28)) 24)
  (best-geodes-f (second (first test-geodes)) 24)
  (best-geodes (second (second test-geodes)) 24)
  (best-geodes-f (second (nth prod-geodes 28)) 24)
  )


(defn get-quality-level [[bp-id blueprints]]
  (prn bp-id)
  (* bp-id (best-geodes-f blueprints 24)))

(defn solve-1
  ([] (solve-1 "resources/2022/day19.txt"))
  ([file]
   (->>
     (h/slurp-strings file)
     (map parse-blueprint)
     (map get-quality-level)
     (reduce +))))


(def test-geodes (map parse-blueprint (h/slurp-strings "resources/2022/day19.test.txt")))
(def prod-geodes (map parse-blueprint (h/slurp-strings "resources/2022/day19.txt")))
;(solve-1)
;(solve-1 "resources/2022/day19.test.txt")


;(->
;  "Blueprint 2: Each ore robot costs 3 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 19 clay. Each geode robot costs 4 ore and 11 obsidian."
;  parse-blueprint
;  (best-geodes 24))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    [1 {:foo {:bar 2} :bar {:foo 1, :baz 12}}] (parse-blueprint "Blueprint 1: Each foo robot costs 2 bar. Each bar robot costs 1 foo and 12 baz.")
    9 (best-geodes (second (first test-geodes)) 24)
    12 (best-geodes (second (second test-geodes)) 24)
    33 (solve-1 "resources/2022/day19.test.txt")
    ))
