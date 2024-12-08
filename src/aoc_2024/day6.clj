(ns aoc-2024.day6
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.tools.trace :refer :all]
   [clojure.string :as str]
   [aoc.helpers :as h]))

(def input "resources/2024/day6.txt")

(def sample
  (str/trim "
  ....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."))

(defn parse-input [s]
  (let [[area dimensions] (-> s (str/split #"\r\n") h/make-xy-map)
        pos (first (filter (comp #{\^} area) (keys area)))]
    {:dimensions dimensions
     :pos pos
     :direction :up
     :visited #{pos}
     :obstacles (set (keys (dissoc area pos)))}))


(def turn-right 
  {:up :rt
   :rt :dn
   :dn :lt
   :lt :up})

(defn went-outside?
  [{:keys [dimensions] :as task} {:keys [x y] :as pos}]
  (or
    (< x 0)
    (< y 0)
    (>= x (:x dimensions))
    (>= y (:y dimensions)) ))

(defn walk-until-outside
  ([task] (walk-until-outside task #{}))
  ([{:keys [direction pos obstacles visited] :as task} turns]

   (let [npos (h/move pos direction)]
     (cond

       (turns [npos direction]) nil

       (obstacles npos) (recur (update task :direction turn-right) (conj turns [pos direction]))

       (went-outside? task npos) visited

       :else (recur (-> task
                      (assoc :pos npos)
                      (update :visited #(conj % npos)))
               turns )))))


(defn boxen-solve
  ([task]
   (let [path (walk-until-outside task)
         ; skip initial pos
         path (disj path (:pos task))]

     (filter (fn [pos] 
               (nil? (walk-until-outside (update task :obstacles #(conj % pos))))) path))))


(comment defn would-loop?
  [{:keys [obstacles pos direction] :as task} visited]
  (let [npos (h/move pos direction)]
    (cond
      (get visited [npos direction]) true
      (get obstacles npos) (recur (update task :direction turn-right) visited)
      (went-outside? task npos) false
      :else (recur 
              (assoc task :pos npos)
              (conj visited [npos direction]) ) ) ) )

(comment defn walk-gather-loops
  ([task] (walk-gather-loops task #{[(:pos task) :up]} []))
  ([{:keys [direction pos obstacles visited] :as task} visited-directions accum]

   (let [npos (h/move pos direction)]
     (cond
       (went-outside? task npos) accum

       (get obstacles npos) (recur 
                              (update task :direction turn-right)
                              (conj visited-directions [pos direction])
                              accum)

       :else (recur
               (-> task
                 (assoc :pos npos)
                 (update :visited #(conj % npos)))
               
               (conj visited-directions [npos direction])

               (if (and
                     (not (visited npos))
                     (would-loop? 
                       (-> task 
                         (update :direction turn-right)
                         (update :obstacles (conj npos)))
                       visited-directions))
                 (conj accum npos)
                 accum))))))

(defn pt1
  [task]
  (count (walk-until-outside task)))

(defn pt2
  [task]
  (count (boxen-solve task)))



(defn solve-1
  ([] (solve-1 (slurp input)))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp input)))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    41 (pt1 (parse-input sample))
    6    (pt2 (parse-input sample))
    1424 (pt2 (parse-input (slurp input)))
    ))

(pt1 (parse-input (slurp "/home/e/draza/karlis-day6/in.text")))
(pt2 (parse-input (slurp "/home/e/draza/karlis-day6/in.text")))
(comment
  (solve-1)
  (solve-2))
