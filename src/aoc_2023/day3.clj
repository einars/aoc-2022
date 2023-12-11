(ns aoc-2023.day3
  (:require
    [clojure.test :as test :refer [deftest]]
    [aoc.helpers :as h]))

(def sample-engine 
  (first (h/make-xy-map 
           ["467..114.."
            "...*......"
            "..35..633."
            "......#..."
            "617*......"
            ".....+.58."
            "..592....."
            "......755."
            "...$.*...."
            ".664.598.."])))


(def digits {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9})

(defn symbol-neighbors [[coords sym]]
  (when-not (digits sym)
    (h/neighbors-8 coords)))

(defn engine->sym-pattern [e]
  (set (mapcat symbol-neighbors e)))


(defn number-starting-coords [e]
  (->> e
    (filterv (fn [[coords sym]]
               (and 
                 (digits sym) 
                 (not (digits (e (h/left-of coords)))))))
    (mapv first))) ; leave only coords


(defn pick-number-at [coords engine touchmap]
  (loop [coords coords 
         touching? false
         accu 0]
    (if-let [d (digits (engine coords))]
      (recur 
        (h/right-of coords) 
        (or touching? (touchmap coords))
        (+ d (* accu 10)))
      (when touching? accu))))

(defn gear-candidates [m]
  (->> m
    (filterv (fn [[_coords sym]] (= sym \*)))
    (mapv first)))

(defn get-gear-ratio [m number-coords gear-coords]
  (let [touchmap (set (h/neighbors-8 gear-coords))
        connected-numbers (->> number-coords
                            (map #(pick-number-at % m touchmap))
                            (filter some?))]
    (when (= 2 (count connected-numbers))
      (reduce * connected-numbers))))


(defn solve-1
  ([] (solve-1 (first (h/slurp-xy-map "resources/2023/day3.txt"))))
  ([m]
   (let [touchmap (engine->sym-pattern m)
         number-coords (number-starting-coords m)]
     (->> number-coords
       (map #(pick-number-at % m touchmap))
       (filter some?)
       (reduce +)))))

(defn solve-2
  ([] (solve-2 (first (h/slurp-xy-map "resources/2023/day3.txt"))))
  ([m]
   (let [gears (gear-candidates m)
         numbermap (number-starting-coords m)
         gears (map #(get-gear-ratio m numbermap %) gears)]
     (reduce + (filter some? gears)))))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    4361 (solve-1 sample-engine)
    467835 (solve-2 sample-engine)
    ))

(comment
  (solve-1)
  (solve-2))
