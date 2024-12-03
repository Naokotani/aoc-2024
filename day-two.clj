(ns advent-of-code.day-two
  (:require
   [clojure.string :as str]))

(def input
  (slurp  "resources/day-two.txt"))

(def reports 
  (map (fn [string]
         (map #(Integer/parseInt %) (str/split string #" ")))
       (str/split input #"\n")))

(defn unsafe-pair? [n1 n2]
  (cond
    (= n1 n2) true
    (> n1 n2) true
    (> (- n2 n1) 3) true
    :else false))

(defn safe-descending? [report]
  (loop [r report]
    (let [n1 (first r) n2 (first (rest r))]
      (cond
        (empty? (rest r)) true
        (unsafe-pair? n2 n1) false
        :else (recur (rest r))))))

(defn safe-ascending? [report]
  (loop [r report]
    (let [n1 (first r) n2 (first (rest r))]
      (cond
        (empty? (rest r)) true
        (unsafe-pair? n1 n2) false
        :else (recur (rest r))))))

(defn safe-report [report]
  (cond
    (safe-ascending? report) true
    (safe-descending? report) true
    :else false))

(defn engage-dampener [report]
  (loop [i 0]
    (if (< i (count report))
      (let [r (vec (concat (subvec report 0 i) (subvec report (inc i))))]
        (cond
          (safe-ascending? r) true
          (safe-descending? r) true
          :else (recur (inc i)))) 
      false)))
    
(defn safe-report-with-damper [report]g
  (cond
    (safe-ascending? report) true
    (safe-descending? report) true
    :else (engage-dampener (into [] report))))
  
(count (filter #(safe-report %) reports))
(count (filter #(safe-report-with-damper %) reports))
