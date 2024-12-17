(ns aoc-2024.day17
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [clojure.tools.trace :refer :all]
   [instaparse.core :as insta]
   [aoc.helpers :as h]))


; Register A: 22571680
; Register B: 0
; Register C: 0
; 
; Program: 
; 2,4 bst4 B = A mod 8
; 1,3 bxl3 B = B xor 3
; 7,5 cdv5 C = A / 2^B
; 0,3 adv3 A = A / 8
; 4,3 bxc_ B = B xor C
; 1,5 bxl5 B = B xor 5
; 5,5 out5 out B mod 8
; 3,0 jnz 0


(def pow2 
  {0 1
   1 2
   2 4
   3 8
   4 16
   5 32
   6 64
   7 128})


; (b = (a mod 8) xor 3
;   c = a / (2^b)
;   a = a / 8
;   b = b ^ c ^ 5
;   out-b

;   b = 0..7
;   0 = (b ^ c ^ 5) mod 8
;   )


(defn revers-one [a exp]
  (filterv some?
    (for [ax (range 8)]
      (let [sa (+ ax (* a 8))
            b (bit-xor (mod sa 8) 3)
            c (quot sa (pow2 b))
            na (quot sa 8)
            b (mod (bit-xor b c 5) 8)]

        (when (and (= na a) (= b exp))
          sa)))))

(defn backtrack 
  [[x & rest] a]
  (if x
    (let [candidates (revers-one a x)]
      (first (filter some? (for [c candidates] (backtrack rest c)))))
    a))

(defn run-prog [a b c accum]
  (let [b (mod a 8)
        b (bit-xor b 3)
        c (quot a (pow2 b))
        a (quot a 8)
        b (bit-xor b c)
        b (bit-xor b 5)
        accum (conj accum (mod b 8))]
    (prn a b c)
    (if (> a 0)
      (recur a b c accum)
      accum)))

;(run-prog 236581645541055 0 0 [])


(defn solve-1 []
  (str/join "," (run-prog 22571680 0 0 [])))

(defn solve-2 []
  (let [target [2,4,1,3,7,5,0,3,4,3,1,5,5,5,3,0]]
    (backtrack (reverse target) 0)))


;(backtrack (reverse [2,0,1,3,4,0,2,1,7]) 0)
;(backtrack (reverse [2,4,1,3,7,5,0,3,4,3,1,5,5,5,3,0]) 0)

(comment
  (solve-1)
  (solve-2))
