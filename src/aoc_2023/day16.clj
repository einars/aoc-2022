(ns aoc-2023.day16
  (:require
    [clojure.test :as test :refer [deftest are]]
    [clojure.string :as str]
    [clojure.set :as set]
    [aoc.helpers :as h]))

(def sample-map (h/slurp-map "resources/2023/day16.sample.txt"))
(def input-file "resources/2023/day16.txt")

(def ^:dynamic *dims* {})

(defn inside-bounds? [coordir]
  (let [{:keys [x y]} (first coordir)
        {:keys [xmin xmax ymin ymax]} *dims*]
    (and
      (<= xmin x xmax)
      (<= ymin y ymax))))


(defn visit [m [coord dir]]
  (condp = (m coord)
    nil [[(h/move coord dir) dir]] ; empty? move on
    \/ (condp = dir
         :lt [[(h/move coord :dn) :dn]]
         :rt [[(h/move coord :up) :up]]
         :up [[(h/move coord :rt) :rt]]
         :dn [[(h/move coord :lt) :lt]])
    \\ (condp = dir
         :lt [[(h/move coord :up) :up]]
         :rt [[(h/move coord :dn) :dn]]
         :up [[(h/move coord :lt) :lt]]
         :dn [[(h/move coord :rt) :rt]])
    \| (condp = dir
         :lt [[(h/move coord :dn) :dn] [(h/move coord :up) :up]]
         :rt [[(h/move coord :dn) :dn] [(h/move coord :up) :up]]
         :up [[(h/move coord :up) :up]]
         :dn [[(h/move coord :dn) :dn]])
    \- (condp = dir
         :lt [[(h/move coord :lt) :lt]]
         :rt [[(h/move coord :rt) :rt]]
         :up [[(h/move coord :lt) :lt] [(h/move coord :rt) :rt]]
         :dn [[(h/move coord :lt) :lt] [(h/move coord :rt) :rt]])))

(defn enlighten 

  ([m] (binding [*dims* (h/map-dimensions m)] (enlighten m [[{:x 0 :y 0} :rt]] {})))

  ([m coordirs visited]


   ; izejam cauri coords un katrai coords pabīdām staru un atzīmējam visited
   ; (coord, visited) -> [coords], new_visited

   (let [[new-coordirs new-visited] (reduce (fn [[coordir-accum visited] coordir]
                                              (if (visited coordir)
                                                [coordir-accum visited] ; neko nedarām
                                                (let [new-coordirs (filter inside-bounds? (visit m coordir))]
                                                  [(concat coordir-accum new-coordirs) (set/union visited (set [coordir]))])))
                                      [[] visited] coordirs )]
     (if (= new-visited visited)
       visited
       (recur m new-coordirs new-visited)))))

(defn print-light [enl]
  (h/print-map (into {} (mapv (fn [x] [x \#]) (map first enl)))))

(print-light (enlighten sample-map))



(defn solve-1
  ([] (solve-1 (h/slurp-map input-file)))
  ([m] (->> m
         enlighten
         (map first)
         set
         count)))

(defn solve-2
  ([] (solve-2 (h/slurp-strings input-file)))
  ([m] (->> m
         )))

(deftest test-stuff [] 
  (are [x y] (= x y)
    46 (solve-1 sample-map)
    0 (solve-2 sample-map)))

(comment
  (solve-1)
  (solve-2))
