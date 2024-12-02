(ns day2.day2
  (:require
   [clojure.string]))

(defn read-reports []
  (->> (slurp "inputs/day2.txt") ; slurp 😋
       (clojure.string/split-lines)
       (mapv #(mapv Integer/parseInt (re-seq #"\d+" %))))) ; split each line into list of ints

(defn safe? [levels]
  (and (or (apply < levels) (apply > levels)) ; monotonically increasing or decreasing
       (->> (partition 2 1 levels) ; pairs
            (map #(apply (comp abs -) %)) ; absolute difference
            (apply max)
            (>= 3)))) ; biggest difference is at most 3

(defn safe-with-damping? [levels]
  (or (safe? levels) ; safe anyway?
      (some true? (for [i (range (count levels))] ; remove ith level, is it safe?
                    (safe? (concat (take i levels) (drop (inc i) levels)))))))

;; part 1
(count (filter safe? (read-reports)))

; part 2
(count (filter safe-with-damping? (read-reports)))