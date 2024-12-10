(ns day10.day10
  (:require
   [clojure.string :as str]))

(defn read-trailmap []
  (->> (slurp "inputs/day10.txt") ; slurp 😋
       (str/split-lines)
       (mapv #(mapv Integer/parseInt (re-seq #"\d" %)))))

;; repurposing day 8 code!!
(defn find-matches [pred row]
  (->> row
       (reduce (fn [{:keys [m i]} h]
                 (if (pred h) {:m (assoc! m i h) :i (inc i)} ; build a map {i h} of indices i where (pred h) is true
                     {:m m :i (inc i)}))
               {:m (transient {})
                :i 0})
       :m            ; only take the map
       persistent!)) ; make it a persistent map

(defn find-trailheads [coll]
  (->> coll
       (reduce (fn [{:keys [t y]} row]
                 {:t (reduce-kv (fn [t' x _]
                                  (conj! t' [x y])) ; build the coords, add to the set of trailheads
                                t (find-matches zero? row))
                  :y (inc y)}) ; find all xs where the antennae are
               {:t (transient #{}) :y 0})
       :t
       persistent!)) ; only take the set

;; day 6 code too!!
(defn at [m [x y]]
  (nth (nth m y) x))

(defn in-bounds? [[width height] [x y]]
  (and (< -1 x width) (< -1 y height)))

(defn neighbours [[width height] [x y]]
  (letfn [(valid-coord? [x' y']
            (in-bounds? [width height] [x' y']))]
    (-> (transient [])
        (cond-> (valid-coord? (dec x) y) (conj! [(dec x) y])
                (valid-coord? (inc x) y) (conj! [(inc x) y])
                (valid-coord? x (dec y)) (conj! [x (dec y)])
                (valid-coord? x (inc y)) (conj! [x (inc y)]))
        persistent!)))

(defn neighbours-at [m [x y]]
  (let [ns (neighbours [(count (first m)) (count m)] [x y])]
    (zipmap ns (map #(at m %) ns))))

(defn filter-neighbours [m height [x y]]
  (persistent! (reduce-kv (fn [t' [x' y'] v]
                            (if (= v height) (conj! t' [x' y']) t'))
                          (transient []) (neighbours-at m [x y]))))

;; part 1
(defn score-trail [m head]
  (loop [heads #{head}
         height 1]
    (cond (= 10 height) (count heads)
          (empty? heads) 0
          :else (recur (persistent! (reduce (fn [t v]
                                              (conj! t v))
                                            (transient #{})
                                            (mapcat #(filter-neighbours m height %) heads)))
                       (inc height)))))

;; part 2
(defn rate-trail [m head]
  (loop [heads [head]
         height 1]
    (cond (= 10 height) (count heads)
          (empty? heads) 0
          :else (recur (persistent! (reduce (fn [t v]
                                              (conj! t v))
                                            (transient [])
                                            (mapcat #(filter-neighbours m height %) heads)))
                       (inc height)))))

(let [trailmap (read-trailmap)]
  (->> trailmap
       (find-trailheads)
       (map #(rate-trail trailmap %))
       (reduce +)))