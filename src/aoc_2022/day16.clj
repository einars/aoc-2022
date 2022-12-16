(ns aoc-2022.day16
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [clojure.math.combinatorics :as combo]
    [aoc.helpers :as h]))

(defn parse [l]
  (when-let [[_ valve rate tunnels] (first (re-seq #"Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? ([A-Z, ]+)" l))]
    [(keyword valve), {:valve (keyword valve)
                       :rate (Integer/parseInt rate)
                       :tunnels (map keyword (str/split tunnels #", "))
                       :distances (into {} (map (fn [t] [t 1]) (map keyword (str/split tunnels #", "))))
                       :open (= "0" rate)}]))

(defn pressure-delta 
  [valves]
  (reduce + (map :rate (filter :open (vals valves)))))

(def max-pressure (atom 0))

(def ^:dynamic *stfu* false)
(defn consider [pressure]
  (when (> pressure @max-pressure)
    (when-not *stfu* 
      (printf "I can get to %d psi\n" pressure)
      (flush))
    (reset! max-pressure pressure)))

(def max-pressure-pt2 (atom 0))

(defn consider-2 [pressure extra]
  (when (> pressure @max-pressure-pt2)
    (printf "PT2 I can get to %d psi %s\n" pressure extra)
    (flush)
    (reset! max-pressure-pt2 pressure)))

(defn build-distances
  [valves]

  (let [dists (atom (into {} (map (fn [v] [(:valve v) (:distances v)]) (vals valves))))]

    (dotimes [_ 5]
      (doseq [v1 (keys valves)]
        (doseq [v2 (keys valves)]
          (doseq [v3 (keys valves)]
            (let [d12 (get-in @dists [v1 v2])
                  d23 (get-in @dists [v2 v3])
                  d13 (get-in @dists [v1 v3])]
              (when (and
                      (not (nil? d12))
                      (not (nil? d23))
                      (or (nil? d13) (> d13 (+ d12 d23))))
                (swap! dists #(assoc-in % [v1 v3] (+ d12 d23)))))))))
    (into {} (map (fn [[k v]] [k (assoc v :distances (@dists k))] ) valves))))

(def valves (->> "resources/2022/day16.test.txt"
              (h/slurp-strings)
              (map parse)
              (into {})))

;(build-distances valves)

(def ^:dynamic *time* 30)

(defn find-max 
  ([valves]
   (reset! max-pressure 0)
   (find-max valves :AA 1 0 #{}))
  ([valves avoid]
   (reset! max-pressure 0)
   (find-max valves :AA 1 0 (set avoid)))
  ([valves loc step pressure avoid]

   (consider (+ pressure (* (pressure-delta valves) (- (inc *time*) step))))

   (let [targets (map :valve (filter #(false? (:open %)) (vals valves)))
         targets (filter #(not (avoid %)) targets)]

     (doseq [t targets]
       (let [distance (get-in valves [loc :distances t])
             distance (inc distance) ; move + close
             new-valves (assoc-in valves [t :open] true)]
         (when (<= (+ step distance) *time*)
           (find-max new-valves t (+ step distance) (+ pressure (* distance (pressure-delta valves))) avoid)))))))

(defn find-max-2
  [valves]

  (reset! max-pressure-pt2 0)

  (binding [*time* 26
            *stfu* true]
    (doseq [[a b] (combo/partitions (map :valve (filter #(false? (:open %)) (vals valves))) :min 2 :max 2)]
      (let [_ (find-max valves a)
            ra @max-pressure
            _ (find-max valves b)
            rb @max-pressure]
        (consider-2 (+ ra rb) [a b])))))


(defn solve-1
  ([] (solve-1 "resources/2022/day16.txt"))
  ([file]
   (let [valves (->> file
                  (h/slurp-strings)
                  (map parse)
                  (into {})
                  (build-distances))]
     (find-max valves)
     @max-pressure)))

(defn solve-2
  ([] "Very slow. Run manually from the repl!")
  ([file]
   (let [valves (->> file
                  (h/slurp-strings)
                  (map parse)
                  (into {})
                  (build-distances))]
     (find-max-2 valves)
     @max-pressure-pt2)))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    true false
    1651 (solve-1 "resources/2022/day16.test.txt")
    1707 (solve-2 "resources/2022/day16.test.txt")))

(comment
  (solve-2 "resources/2022/day16.txt"))
