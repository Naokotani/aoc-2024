(ns advent-of-code.day-two
  (:require
   [clojure.string :as str]))

(def input
  (slurp  "resources/day-three.txt"))

(defn string->list []
  (map str input))

(defn build-enabled [char-list]
  (loop [list char-list enable-list (vector)]
    (let [e (conj enable-list (first list))]
      (cond
        (= (first list) ")") (conj enable-list (first list))
        (not (re-matches #"^[don't()]$" (first list))) '()
        (empty? (rest list)) '()
        :else (recur (rest list) e)))))

(defn parse-enabled [enabled]
  (let [e (apply str enabled)]
    (cond
      (re-matches #"^do\(\)$" (apply str enabled)) '(true)
      (re-matches #"^don't\(\)$" (apply str enabled)) '(false)
      :else '())))

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
      (cond
        (= (first list) ")") (conj mul-list (first list))
        (not (re-matches #"^[mul(),0-9]$" (first list))) '()
        (empty? (rest list)) '()
        :else (recur (rest list) m)))))

(defn parse-mul [list]
  (if (not (empty? (build-mul list)))
    (mul-result (build-mul list))
    0))

(defn parse-corrupted-string-with-do [list]
  (loop [char-list list total 0 enabled '(true)]
    (cond
      (= "m" (first char-list)) (recur (rest char-list)
                                       (if (= (first enabled) true)
                                         (+ (parse-mul char-list) total)
                                         total)
                                       enabled)
      (= "d" (first char-list)) (recur
                                 (rest char-list)
                                 total
                                 (if (not-empty (parse-enabled (build-enabled char-list)))
                                                                (parse-enabled (build-enabled char-list))
                                                                enabled))
      (empty? (rest char-list)) total
      :else (recur (rest char-list) total enabled))))

(defn parse-corrupted-string [list]
  (loop [char-list list total 0]
    (cond
      (= "m" (first char-list)) (recur (rest char-list) (+ (parse-mul char-list) total))
      (empty? (rest char-list)) total
      :else (recur (rest char-list) total))))

(comment
  
  (println (parse-corrupted-string (string->list)))
  (println (parse-corrupted-string-with-do (string->list)))

  )
    

