(ns day7.day7
  (:require
   [clojure.string]))

(defn operation-tree [fs xs]
  (reduce (fn [coll f]
            (into coll (map f xs)))
          [] fs))

(defn possible-answers [vals]
  (reduce (fn [xs x]
            (operation-tree [(partial * x) (partial + x)] xs))
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
