(ns day1.day1
  (:require
   [clojure.string]))

(defn get-two-lists
  []
  (->> (slurp "inputs/day1.txt") ; slurp 😋
       (clojure.string/split-lines)
       (mapv #(mapv Integer/parseInt (re-seq #"\d+" %))) ; split each line into [int int]
       (apply mapv list))) ; transpose

(defn count-occurrences
  [xs]
  (reduce (fn [counts x]
            (update counts x (fnil inc 0)))
          {} ; counts
          xs))

;; part 1
(->> (get-two-lists)
     (mapv sort)
     (apply map (comp abs -)) ; get differences
     (reduce +)) ; sum

;; part 2
(->> (get-two-lists)
     (apply (fn [xs ys]
              (let [y-counts (count-occurrences ys)] ; get counts of values in second list
                (for [x xs] (* x (get y-counts x 0)))))) ; multiply each value in first list by count in second
     (reduce +)) ; sum
