(ns aoc-2022.helpers
  (:require [clojure.java.io :as io]))

(defn slurp-strings [file]
  (with-open [rdr (io/reader file)]
    (reduce conj [] (line-seq rdr))))

(defn slurp-ints [file]
  (mapv #(Integer/parseInt %) (slurp-strings file)))

