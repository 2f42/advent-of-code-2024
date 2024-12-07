(ns day7.day7
  (:require
   [clojure.string]))

;; part 1
(defn possible-answers [vals]
  (reduce (fn [coll x] ; build a collection of possible answers
            (into (map (partial * x) coll) ; multiply x through coll
                  (map (partial + x) coll))) ; add x through coll
          [(first vals)] (rest vals)))

;; part 2
(defn possible-answers-2 [vals]
  (reduce (fn [coll x] ; build a collection of possible answers
            (->> (map (partial * x) coll) ; multiply x through coll
                 (into (map (partial + x) coll)) ; add x through coll
                 (into (map (fn [a]
                              (parse-long (str (str a) (str x)))) coll)))) ; concat numbers
          [(first vals)] (rest vals)))

(->> (slurp "inputs/day7.txt") ; slurp 😋
     (clojure.string/split-lines)
     (mapv #(mapv parse-long (re-seq #"\d+" %))) ; extract numbers
     (map #(list (first %) (possible-answers-2 (rest %)))) ; first number of output is target, second is all reachable answers
     (filter #(some #{(first %)} (second %))) ; filter by results that have the target in reachable answers
     (map first) ; take the targets
           (reduce +))) ; sum

;; part 2 one line monstrosity
(reduce + (map first (filter #(some #{(first %)} (second %)) (map #(list (first %) (reduce (fn [coll x] (into (into (map (partial * x) coll) (map (partial + x) coll)) (map (fn [a] (parse-long (str (str a) (str x)))) coll))) [(second %)] (drop 2 %))) (mapv #(mapv parse-long (re-seq #"\d+" %)) (clojure.string/split-lines (slurp "inputs/day7.txt")))))))