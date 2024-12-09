(ns day8.day8
  (:require
   [clojure.string :as str]))

(defn read-map []
  (->> (slurp "inputs/day8.txt") ; slurp 😋
       str/split-lines))

(defn find-matches [pred s]
  (->> s
       (reduce (fn [{:keys [m i]} c]
                 (if (pred c) {:m (assoc! m i c) :i (inc i)} ; build a map {i c} of indices i where (pred c) is true
                     {:m m :i (inc i)}))
               {:m (transient {})
                :i 0})
       :m            ; only take the map
       persistent!)) ; make it a persistent map

(defn find-antennas [coll]
  (->> coll
       (reduce (fn [{:keys [m y]} row]
                 {:m (reduce (fn [m' [x c]]
                               (update m' c (fnil conj #{}) [x y])) ; build the coords, add to the set of antennae for antenna c
                             m (find-matches #(re-matches #"[a-zA-Z0-9]" (str %)) row))
                  :y (inc y)}) ; find all xs where the antennae are
               {:m {} :y 0})
       :m)) ; only take the map

(defn in-bounds? [[width height] [x y]]
  (and (< -1 x width) (< -1 y height)))

(defn get-offsets [[width height] coord offset]
  (take-while #(in-bounds? [width height] %) ; while in bounds (for part 1, change this to take 1 instead of take-while)
              (iterate #(mapv + offset %) coord))) ; repeatedly add the offset to the coordinate

(defn find-antinodes [[width height] coords]
  (->> (mapv (fn [a]
               (->> (mapv #(mapv - a %) coords)
                    (filter (partial not= '(0 0))))) coords) ; remove self
       (mapcat (fn [coord offsets]
                 (->> offsets
                      (mapcat #(get-offsets [width height] coord %)))) coords))) ; concatenate the results

(let [city (read-map)
      width (count (first city))
      height (count city)]
  (->> city
       find-antennas
       (reduce (fn [m a]
                 (->> (find-antinodes [width height] (second a))
                      (assoc! m (first a))))
               (transient {}))
       persistent!     ; make m persistent
       (mapcat second) ; only take the values from the map
       distinct        ; remove duplicates
       count))