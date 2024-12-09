(ns aoc-2024.day9
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [clojure.tools.trace :refer :all]
   [aoc.helpers :as h]))

(def input-txt "resources/2024/day9.txt")

(def sample
  "2333133121414131402")

(defn load-disk
  [data]
  (loop [[[n-taken n-free] & rest] data, disk [], file-id 0, pos 0]
    (let [disk (if (> n-taken 0) (conj disk {:len n-taken :pos pos :id file-id}) disk)
          disk (if (> n-free 0) (conj disk {:len n-free :pos (+ pos n-taken) :id nil}) disk)]
      (if rest
        (recur rest disk (inc file-id) (+ pos n-taken n-free))
        disk))))

(defn parse-input
  [s]
  (load-disk (partition 2 2 [0 0] (mapv (comp parse-long str) (str/trim s)))))

(defn repr [disk]
  (apply str (for [f (sort-by :pos disk)]
               (apply str (repeat (:len f) (or (:id f) "."))))))


(defn find-free-space 
  [disk]
  (some (fn [x] (when-not (:id x) x)) disk))

(defn fill-with-something 
  [free-space disk]
  (let [last-file (some (fn [f] (when (:id f) f)) (reverse (sort-by :pos disk)))
        orig-disk disk
        disk (filterv (complement #{last-file}) disk)

        len-file (:len last-file)
        len-space (:len free-space)]

    (cond
      (> (:pos free-space) (:pos last-file)) orig-disk ; all done

      (= len-file len-space) (conj disk (assoc last-file :pos (:pos free-space))) ; move file exactly

      ; file too big, move as much as possible
      (> len-file len-space) (-> disk
                               (conj (assoc last-file :pos (:pos free-space) :len len-space)) 
                               (conj (assoc last-file :len (- len-file len-space))))

      ; huge amounts of free space
      (< len-file len-space) (-> disk
                               (conj (assoc last-file :pos (:pos free-space)))
                               (conj (assoc free-space 
                                       :pos (+ (:pos free-space) len-file)
                                       :len (- (:len free-space) len-file)))))))

(defn defrag [disk]
  ;(println (repr disk))
  (if-let [free-space (find-free-space disk)]
    (recur (fill-with-something free-space (filterv (complement #{free-space}) disk)))
    (sort-by :pos disk)))

(defn move-file 
  [disk file space]
  (let [len-file (:len file)
        len-space (:len space)
        disk (filterv (complement #{file space}) disk)]
    (cond
      (= len-file len-space) (conj disk (assoc file :pos (:pos space)))
      (< len-file len-space) (sort-by :pos (-> disk
                                             (conj (assoc file :pos (:pos space)))
                                             (conj (assoc space 
                                                     :pos (+ (:pos space) len-file)
                                                     :len (- (:len space) len-file) )) ) ) )))

(defn find-better-place 
  [disk file-id]
  (let [file (first (filterv #(= (:id %) file-id) disk))
        matching-space (first (filter (fn [space] (and (nil? (:id space)) 
                                                    (>= (:len space) (:len file))
                                                    (< (:pos space) (:pos file))))
                                disk))]
    (if matching-space
      (move-file disk file matching-space)
      disk)))

(defn defrag-pt2 [disk]
  ;(println (repr disk))
  (reduce find-better-place disk (range (:id (last disk)) -1 -1)))

(defn file-score [{:keys [pos id len] :as _file}]
  (reduce + (mapv (fn [l] (* (+ l pos) (or id 0))) (range len))))

(defn score [disk]
  (reduce + (map file-score disk)))


(defn pt1
  [task]
  (score (defrag task)))

(defn pt2
  [task]
  (score (defrag-pt2 task)))

(defn solve-1
  ([] (solve-1 (slurp input-txt)))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp input-txt)))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    1928 (pt1 (parse-input sample))
    2858 (pt2 (parse-input sample))))

(comment
  (solve-1)
  (solve-2))
