(ns day8.version2
  (:require
   [clojure.string]))

(defn read-map []
  (->> (slurp "inputs/day8.txt") ; slurp 😋
       (clojure.string/split-lines)))

(defn find-matches [pred s]
  (->> s
       (reduce (fn [{:keys [m i]} c]
                 (if (pred c) {:m (assoc! m i c) :i (inc i)} ; build a map {i c} of indices i where (pred c) is true
                     {:m m
                      :i (inc i)}))
               {:m (transient {}) :i 0})
       (:m) ; only take the map
       (persistent!))) ; make it a persistent map

(defn find-antennas [city]
  (->> city
       (reduce (fn [{:keys [m y]} row]
                 {:m (reduce-kv (fn [m' x c]
                                  (update m' c (fnil conj #{}) [x y])) ; build the coords, add to the set of antennae for antenna c
                                m (find-matches #(re-matches #"[a-zA-Z0-9]" (str %)) row))
                  :y (inc y)}) ; find all xs where the antennae are
               {:m {} :y 0})
       (:m))) ; only take the map

(defn in-bounds? [[width height] [x y]]
  (and (<= 0 x) (> width x) (<= 0 y) (> height y)))

(defn get-offsets [[width height] coord offset]
  (take-while #(in-bounds? [width height] %) ; while in bounds (for part 1, take 1)
              (iterate #(mapv + offset %) coord))) ; repeatedly add the offset to the coordinate

(defn find-antinodes [[width height] coords]
  (->> coords
       (mapv (fn [a]
               (->> (mapv #(mapv - a %) coords)
                    (remove #(= '(0 0) %))))) ; remove self 
       (mapcat (fn [coord offsets]
                 (->> offsets
                      (mapcat #(get-offsets [width height] coord %)))) ; concatenate the results
               coords)))

(let [city (read-map)
      width (count (first city))
      height (count city)]
  (->> city
       (find-antennas)
       (reduce-kv (fn [m a coords]
                    (->> (find-antinodes [width height] coords)
                         (assoc! m a)))
                  (transient {}))
       (persistent!) ; make m persistent 
       (mapcat second) ; only take the values from the map
       (distinct) ; remove duplicates
       (count))) ; count