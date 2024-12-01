(ns advent-of-code.day-one
  (:require
   [clojure.string :as str]))

(def input
  (slurp  "resources/puzzle.txt"))

(def list-one
  (sort < (map #(Integer/parseInt %) (map #(first (str/split % #" ")) (str/split input #"\n")))))

(def list-two
  (sort < (map #(Integer/parseInt %) (map #(last (str/split % #" ")) (str/split input #"\n")))))

(defn get-distance [num1 num2]
  (if (neg-int? (- num1  num2))
    (* -1 (- num1 num2))
    (- num1 num2)))

(defn similarity-score [num list]
  (* num (count (filter #(= num %) list))))

(defn question-one []
  (loop [l1 list-one
         l2 list-two
       result 0]
  (if (empty? l1)
    (println result)
    (recur
     (rest l1)
     (rest l2)
     (+ result (get-distance
                (first l1)
                (first l2)))))))

(defn question-two []
  (loop [l1 list-one
         score 0]
    (if (empty? l1)
      (println score)
      (recur (rest l1)
             (+ score (similarity-score
                       (first l1)
                       list-two))))))

