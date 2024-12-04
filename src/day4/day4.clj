(ns day4.day4
  (:require
   [clojure.string]))

(defn read-board []
  (->> (slurp "inputs/day4.txt") ; slurp 😋
       (clojure.string/split-lines)))

(defn rotate-board
  ([board]
   (->> board
        (reverse) ; flip vertically
        (apply map str))) ; transpose
  ([board times]
   (case times
     0 board
     (rotate-board (rotate-board board) (dec times)))))

(defn not-in-right-n-columns? [index size n]
  (>= size (+ (mod index size) (inc n))))

(defn horizontal-matches [board]
  (->> board
       (map #(re-seq #"XMAS" %))
       (remove nil?)
       (map count)
       (apply +)))

(defn diagonal-matches [board]
  (let [size (count board)
        board-string (apply str board)]
    (->> (for [i (range (* size size))
               :when (not-in-right-n-columns? i size 3)]
           (re-matches #"X.{140}M.{140}A.{140}S.*" (subs board-string i)))
         (remove nil?) ; remove non-matches
         (count))))

(defn x-mas-matches [board]
  (let [size (count board)
        board-string (apply str board)]
    (->> (for [i (range (* size size))
               :when (not-in-right-n-columns? i size 2)]
           (re-matches #"M.S.{138}A.{138}M.S.*" (subs board-string i)))
         (remove nil?) ; remove non-matches
         (count))))

;; part 1
(let [board (read-board)]
  (->> (for [i (range 4)]
         (rotate-board board i))
       (map #(+ (horizontal-matches %) (diagonal-matches %)))
       (reduce +)))

;; part 2
(let [board (read-board)]
  (->> (for [i (range 4)]
         (rotate-board board i))
       (map x-mas-matches)
       (reduce +)))

