(ns day8.day8
  (:require
   [clojure.string]))

(defn read-map []
  (->> (slurp "inputs/day8-example.txt") ; slurp 😋
       (clojure.string/split-lines)))

(defn find-matches [pred s]
  (->> s
       (reduce (fn [[m i] c]
                 (if (pred c) [(assoc! m i c) (inc i)]
                     [m (inc i)]))
               [(transient {}) 0])
       (first)
       (persistent!)))

(defn find-antennas [coll]
  (->> coll
       (reduce (fn [[m y] row]
                 [(reduce (fn [m' [x c]]
                            (update m' c (fnil conj #{}) [x y]))
                          m (find-matches #(re-matches #"[a-zA-Z0-9]" (str %)) row)) (inc y)])
               [{} 0])
       (first)))

(defn in-bounds? [[width height] [x y]]
  (and (<= 0 x) (> width x) (<= 0 y) (> height y)))

(defn get-offsets [[width height] coord offset]
  (take-while #(in-bounds? [width height] %)
              (iterate #(mapv + offset %) coord)))

(defn find-antinodes [[width height] coords]
  (->> (mapv (fn [a]
               (->> (mapv #(mapv - a %) coords)
                    (filter (partial not= '(0 0))))) coords)
       (mapcat (fn [coord offsets]
                 (->> offsets
                      (mapcat #(get-offsets [width height] coord %)))) coords)))

(let [city (read-map)
      width (count (first city))
      height (count city)]
  (->> city
       (find-antennas)
       (reduce (fn [m a]
                 (->> (find-antinodes [width height] (second a))
                      (filterv #(in-bounds? [width height] %))
                      (assoc! m (first a))))
               (transient {}))
       (persistent!)
       (mapcat second)
       (reduce conj #{})
       (count)))