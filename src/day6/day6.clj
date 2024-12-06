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

(defn traverse [lab width height starting-position]
  (reduce (fn [[lab width height [x y] facing seen] _]
            (cond (out-of-bounds? width height (step x y facing)) (reduced [false (update seen [x y] (fnil conj #{}) facing)]) ; left the bounds
                  (contains? (seen [x y]) facing) (reduced [true (update seen [x y] (fnil conj #{}) facing)]) ; been in this position before, exit
                  :else (case (apply at lab (step x y facing))
                          \# [lab width height [x y] (turn facing) seen] ; turn if hits an obstacle
                          [lab width height (step x y facing) facing (update seen [x y] (fnil conj #{}) facing)]))) ; otherwise, step!
          [lab width height starting-position :up {}]
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
      starting-position (find-guard lab)
      in-path (->> (traverse lab width height starting-position)
                   (second)
                   (keys))] ; get the set of seen positions
  (->> (for [x (range width)
             y (range height)
             :when (and (some #{[x y]} in-path) ; make sure (x,y) is on the path 
                        (not= starting-position [x y]))] ; and its not the start position
         (update lab y #(str (subs % 0 x) "#" (subs % (inc x)))))
       (filter #(->> (traverse % width height starting-position)
                     (first))) ; only take paths that loop
       (count)))