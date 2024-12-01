(ns aoc-2023.day17
  (:require
    [clojure.test :as test :refer [deftest are]]
    [clojure.string :as str]
    [aoc.helpers :as h]))

(def sample-file "resources/2023/day17.sample.txt")
(def input-file "resources/2023/day17.txt")

(def next-states 
  {:init [:u1 :d1 :l1 :r1]
   :u1 [:u2     :l1 :r1]
   :u2 [:u3     :l1 :r1]
   :u3 [        :l1 :r1]
   :d1 [    :d2 :l1 :r1]
   :d2 [    :d3 :l1 :r1]
   :d3 [        :l1 :r1]
   :l1 [:u1 :d1 :l2    ]
   :l2 [:u1 :d1 :l3    ]
   :l3 [:u1 :d1        ]
   :r1 [:u1 :d1     :r2]
   :r2 [:u1 :d1     :r3]
   :r3 [:u1 :d1        ]})

(defn moved [xy dir]
  (case dir
    :u1 (h/top-of xy)
    :u2 (h/top-of xy)
    :u3 (h/top-of xy)
    :l1 (h/left-of xy)
    :l2 (h/left-of xy)
    :l3 (h/left-of xy)
    :r1 (h/right-of xy)
    :r2 (h/right-of xy)
    :r3 (h/right-of xy)
    :d1 (h/bottom-of xy)
    :d2 (h/bottom-of xy)
    :d3 (h/bottom-of xy)))



(defn heat->int [c]
  (- (int c) (int \0)))

(defn get-next-states [m {:keys [heat state coord]}]

  (for [next-state (next-states state)
        :let [new-coord (moved coord next-state)]
        :when (m new-coord)]

    {:heat (+ heat (heat->int (m new-coord)))
     :state next-state
     :coord new-coord}))

(defn improves-result? [bests {:keys [heat state coord]}]
  (if-let [current-best (get-in bests [coord state])]
    (< heat current-best)
    true))

(defn update-best [bests {:keys [heat state coord]}]
  ;(prn :update-best bests :heat heat :state state :coord coord)
  (assoc-in bests [coord state] heat))

(defn pathfind 
  ([m] (pathfind m [{:coord {:x 0 :y 0} :heat 0 :state :init}] {}))
  ([m front bests]
   ;(prn (->> front (mapv #(get-next-states m %))))
   (if (empty? front)
     bests
     (let [good-candidates (->> (mapcat #(get-next-states m %) front)
                             (filterv #(improves-result? bests %)))]
       (prn good-candidates)
       (recur m good-candidates (reduce update-best bests good-candidates))))))



(def sm (h/string->map "123\n456\n789"))
;(def sm (first (h/slurp-xy-map sample-file)))

(pathfind sm)

(defn solve-1
  ([] (solve-1 input-file))
  ([m] (->> m
         h/slurp-xy-map
         first
         )))

(defn solve-2
  ([] (solve-2 (h/slurp-strings input-file)))
  ([m] (->> m
         )))

(deftest test-stuff [] 
  (are [x y] (= x y)
    0 (solve-1 sample-file)
    0 (solve-2 sample-file)))

(comment
  (solve-1)
  (solve-2))
