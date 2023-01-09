(ns aoc.core-test
  (:require [clojure.test :refer [run-tests]]))

(apply run-tests (for [y (range 2015 2023)
                       d (range 26)
                       :let [n (symbol (format "aoc-%d.day%d" y d))]
                       :when (try (require n) (ns-publics n) (catch Exception _ nil))]
                   n))






