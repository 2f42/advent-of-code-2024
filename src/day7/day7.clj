(ns day7.day7
  (:require
   [clojure.string]))

(defn possible-answers [vals]
  (reduce (fn [xs x]
            (into (map (partial * x) xs)
                  (map (partial + x) xs)))
          [(first vals)] (rest vals)))

(defn possible-answers-2 [vals]
  (reduce (fn [xs x]
            (->> (map (partial * x) xs)
                 (into (map (partial + x) xs))
                 (into (map (fn [a]
                              (parse-long (str (str a) (str x)))) xs))))
          [(first vals)] (rest vals)))

(->> (slurp "inputs/day7.txt") ; slurp 😋
     (clojure.string/split-lines)
     (mapv #(mapv parse-long (re-seq #"\d+" %)))
     (map #(list (first %) (possible-answers-2 (rest %))))
     (filter #(some #{(first %)} (second %)))
     (map first)
     (reduce +))
