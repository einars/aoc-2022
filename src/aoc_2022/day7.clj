(ns aoc-2022.day7
  (:require [clojure.test :as test :refer [deftest]])
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pprint])
  (:require [clojure.tools.trace :as t :refer [trace deftrace]])
  (:require [aoc.helpers :as h]))

(def blank-hdd
 {:pwd []
  :dirsizes { [] 0 }})

(defn exec-cd
  [hdd path]
  (condp = path
    "/" (assoc hdd :pwd [])
    ".." (update hdd :pwd pop)
    (update hdd :pwd #(conj % path))))

(defn add-dir-at-pwd
  [hdd dirname]
  (assoc-in hdd [:dirsizes (conj (:pwd hdd) dirname)] 0))

(defn add-file-at-pwd
  [hdd _file-name file-size]
  (loop [path (:pwd hdd) hdd hdd]
    (if (seq path)
      (recur
        (pop path)
        (update-in hdd [:dirsizes path] #(+ % file-size)))
      (update-in hdd [:dirsizes []] #(+ % file-size)))))

(defn apply-cmd 
  [hdd cmd]
  (let [cmd (if (str/starts-with? cmd "$") (subs cmd 2) cmd)
        parts (str/split cmd #" ")]
    (condp = (first parts)
      "ls" hdd
      "cd" (exec-cd hdd (second parts))
      "dir" (add-dir-at-pwd hdd (second parts))
      (add-file-at-pwd hdd (second parts) (Integer/parseInt (first parts))))))

(defn get-dir-size [hdd dir] (get-in hdd [:dirsizes dir]))

(defn exec-cmds [cmds]
  (reduce apply-cmd blank-hdd cmds))


(defn solve-1
  ([] (solve-1 "resources/2022/day7.txt"))
  ([file]
    (->>
      (h/slurp-strings file)
      exec-cmds
      :dirsizes
      vals
      (filter #(<= % 100000))
      (reduce +))))

(comment defn solve-2
  ([] (solve-2 "resources/2022/day7.txt"))
  ([file]
    (->>
      (h/slurp-strings file)
      ; ...
      )))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    48381165 (get-dir-size (exec-cmds (h/slurp-strings "resources/2022/day7.test.txt")) [])
    94853 (get-dir-size (exec-cmds (h/slurp-strings "resources/2022/day7.test.txt")) ["a"])
    95437 (solve-1 "resources/2022/day7.test.txt")
    ; 0 (solve-2 "resources/2022/day7.test.txt")
    ))


