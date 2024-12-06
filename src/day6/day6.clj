(ns day6.day6
  (:require
   [clojure.string]))

(defn read-lab []
  (->> (slurp "inputs/day6.txt") ; slurp 😋
       (clojure.string/split-lines)))

(defn find-guard [lab]
  (let [y (reduce (fn [i row]
                    (if (clojure.string/includes? row "^") (reduced i) ; is guard in this row? brilliant
                        (inc i)))
                  0 lab)
        x (clojure.string/index-of (lab y) \^)] ; find position of guard in row
    [x y]))

(defn out-of-bounds? [width height [x y]]
  (or (> 0 x) (<= width x) (> 0 y) (<= height y)))

(defn at [lab x y]
  (nth (lab y) x))

(defn turn [facing]
  (case facing
    :up :right
    :right :down
    :down :left
    :left :up))

(defn step [x y facing]
  (case facing
    :up [x (dec y)]
    :down [x (inc y)]
    :left [(dec x) y]
    :right [(inc x) y]))

(defn traverse [lab width height position]
  (reduce (fn [[[x y] facing seen] _]
            (cond (out-of-bounds? width height (step x y facing)) (reduced [false (update seen [x y] (fnil conj #{}) facing)]) ; left the bounds
                  (contains? (seen [x y]) facing) (reduced [true (update seen [x y] (fnil conj #{}) facing)]) ; been in this position before
                  :else (case (apply at lab (step x y facing))
                          \# [[x y] (turn facing) seen] ; turn if hits an obstacle
                          [(step x y facing) facing (update seen [x y] (fnil conj #{}) facing)]))) ; otherwise, step!
          [position :up {}]
          (cycle '(nil))))

;; part 1
(let [lab (read-lab)
      width (count (first lab))
      height (count lab)
      starting-position (find-guard lab)]
  (->> (traverse lab width height starting-position)
       (second)
       (count)))

;; part 2 (a bit slow, might improve later)
(let [lab (read-lab)
      width (count (first lab))
      height (count lab)
      starting-position (find-guard lab)]
  (->> (traverse lab width height starting-position)
       (second)
       (keys) ; get the set of seen positions
       (map (fn [[x y]]
              (update lab y #(str (subs % 0 x) "#" (subs % (inc x)))))) ; get a new lab with the obstacle in place
       (filter #(first (traverse % width height starting-position))) ; only take infinite cycles
       (count)))