(ns advent-of-code.day-two
  (:require
   [clojure.string :as str]))

(def input
  (slurp  "resources/day-three.txt"))

input


(defn string->list []
  (map str input))

(string->list)

(defn mul? [mul]
  (re-matches #"^mul\(\d+,\d+\)$" (apply str mul)))

(defn mul-string->list [mul]
  (if (mul? mul)
    (map #(Integer/parseInt %) (str/split (apply str (drop-last (drop 4 mul))) #","))
    '()))

(defn mult-mul-list [mul]
  (if (not-empty mul)
    (* (first mul) (last mul))
    0))


(defn mul-result [mul]
  (mult-mul-list (mul-string->list (mul? mul))))

(defn build-mul [char-list]
  (loop [list char-list mul-list (vector)]
    (let [m (conj mul-list (first list))]
      (println m)
      (cond
        (= (first list) ")") (conj mul-list (first list))
        (not (re-matches #"^[mul(),0-9]$" (first list))) '()
        (empty? (rest list)) '()
        :else (recur (rest list) m)))))

(defn parse-mul [list]
  (if (not (empty? (build-mul list)))
    (mul-result (build-mul list))
    0))

(defn parse-corrupted-string [list]
  (loop [char-list list total 0]
    (println total)
    (cond
      (= "m" (first char-list)) (recur (rest char-list) (+ (parse-mul char-list) total))
      (empty? (rest char-list)) total
      :else (recur (rest char-list) total))))


(comment
  (mul? '("m" "u" "l" "(" "1" "," "2" ")"))
  (mul-string->list (mul? '("m" "u" "l" "(" "1" "," "2" ")")))
  (mult-mul-list (mul-string->list (mul? '("m" "u" "l" "(" "1" "," "2" ")"))))
  (mul-result '("m" "u" "l" "(" "1" "," "2" ")"))

  (parse-corrupted-string (string->list))
  )
    

