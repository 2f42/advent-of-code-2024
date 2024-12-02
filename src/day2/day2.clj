(ns day2.day2
  (:require
   [clojure.string]))

(defn read-reports
  []
  (->> (slurp "inputs/day2.txt") ; slurp 😋
       (clojure.string/split-lines)
       (mapv #(mapv Integer/parseInt (re-seq #"\d+" %))))) ; split each line into list of ints

(defn safe?
  [levels]
  (and
   (or (apply < levels) (apply > levels)) ; monotonically increasing or decreasing
   (->> levels
        (partition 2 1) ; pairs
        (map #(apply (comp abs -) %)) ; absolute difference
        (apply max)
        (>= 3))))

(defn safe-with-damping?
  [levels]
  (or (safe? levels) ; safe anyway?
      (some true? (for [i (range (count levels))] ; remove ith level, is it safe?
                    (safe? (concat (take i levels) (nthrest levels (inc i))))))))

;; part 1
(reduce (fn [total levels]
          (if (safe? levels)
            (do (println levels)
                (inc total))
            total))
        0
        (read-reports))

; part 2
(reduce (fn [total levels]
          (if (safe-with-damping? levels)
            (do (println levels)
                (inc total))
            total))
        0
        (read-reports))