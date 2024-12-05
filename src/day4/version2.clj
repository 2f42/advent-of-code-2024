(ns day4.version2
  (:require
   [clojure.string]))

(defn read-board []
  (->> (slurp "inputs/day4.txt") ; slurp 😋
       (clojure.string/split-lines)))

(defn rotate-board [board]
  (->> board
       (reverse) ; flip vertically
       (apply map str))) ; transpose

(defn not-in-right-n-columns? [index size n]
  (>= size (+ (mod index size) (inc n))))

(defn count-matches [re margin board]
  (let [size (count board)
        board-string (apply str board)]
    (->> (for [i (range (* size size))
               :when (not-in-right-n-columns? i size margin)]
           (re-matches re (subs board-string i)))
         (remove nil?) ; remove non-matches
         (count))))

;; part 1
(->> (read-board)
     (iterate rotate-board)
     (take 4)
     (map #(+ (count-matches #"XMAS.*" 3 %) (count-matches #"X.{140}M.{140}A.{140}S.*" 3 %)))
     (reduce +))

;; part 2
(->> (read-board)
     (iterate rotate-board)
     (take 4)
     (map #(count-matches #"M.S.{138}A.{138}M.S.*" 2 %))
     (reduce +))
