; 2022 day 21 with some ugly codegen

(require '[clojure.string :as str])

(defn parse-math [m] 
  (try
    (Integer/parseInt m)
    (catch Exception _
      (let [[_ a op b] (first (re-seq #"([a-z]+) ([+\-*\/]+) ([a-z]+)" m))]
        [(symbol op) (symbol a) (symbol b)]))))

(defn parse [s]
  (let [[name rest] (str/split s #": ")]
    [(symbol name) (parse-math rest)] ) )


(defn solve-1 [lines]

  (let [parsed (mapv parse lines)]
    ; forward-defs
    (doseq [[name _] parsed] (eval `(def ~name)))
    ; actual-defs
    (doseq [[name code] parsed] (eval `(defn ~name
                                         [] 
                                         ~(if (number? code)
                                            code
                                            `(~(first code) (~(second code)) (~(nth code 2)))))))
    (eval `(root))))

(prn (solve-1 (str/split (slurp "/proj/aoc/resources/2022/day21.txt") #"\n")))

