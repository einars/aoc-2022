(ns aoc-2022.helpers
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io]))

(defn slurp-strings [file]
  (with-open [rdr (io/reader file)]
    (reduce conj [] (line-seq rdr))))

(defn slurp-blocks [file]
  (mapv #(str/split % #"\n")
    (str/split (slurp file) #"\n\n")))


(defn slurp-ints [file]
  (mapv #(Integer/parseInt %) (slurp-strings file)))

