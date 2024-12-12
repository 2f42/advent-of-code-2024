(ns day12.day12
  (:require
   [clojure.string :as str]))

(defn read-garden []
  (->> (slurp "inputs/day12-example.txt") ; slurp 😋
       str/split-lines
       (mapv char-array)))

;; day 10 code reuse mmm
(defn at [m [x y]]
  (nth (nth m y) x))

(defn in-bounds? [[width height] [x y]]
  (and (< -1 x width) (< -1 y height)))

(defn possible-neighbours [x y]
  (-> (transient [])
      (conj! [(dec x) y])
      (conj! [(inc x) y])
      (conj! [x (dec y)])
      (conj! [x (inc y)])
      persistent!))

(defn possible-neighbours-8 [x y]
  (-> (transient [])
      (conj! [(dec x) (dec y)])
      (conj! [(dec x) y])
      (conj! [(dec x) (inc y)])
      (conj! [(inc x) (dec y)])
      (conj! [(inc x) y])
      (conj! [(inc x) (inc y)])
      (conj! [x (dec y)])
      (conj! [x (inc y)])
      persistent!))

(defn offsets [[x y] coords]
  (persistent! (reduce (fn [t [x' y']]
                         (conj! t [(- x' x) (- y' y)]))
                       (transient []) coords)))

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

(defn filter-neighbours [m plant [x y]]
  (persistent! (reduce-kv (fn [t' [x' y'] v]
                            (if (= v plant) (conj! t' [x' y']) t'))
                          (transient []) (neighbours-at m [x y]))))

(defn build-region [garden [x y] plant]
  (loop [region #{[x, y]}
         queue [[x y]]]
    (if (empty? queue) region
        (let [queue' (distinct (remove #(contains? region %) (mapcat #(filter-neighbours garden plant %) queue)))]
          (recur (into region queue') queue')))))

(defn build-regions [garden]
  (let [width (count (first garden))
        height (count garden)]
    (loop [regions #{}
           seen #{}
           [x y] [0 0]
           plant (at garden [x y])]
      (let [[x' y'] (if (in-bounds? [width height] [(inc x) y]) [(inc x) y] [0 (inc y)])]
        (cond (not (in-bounds? [width height] [x' y'])) regions
              (contains? seen plant) (recur regions seen [x' y'] (at garden [x' y']))
              :else (let [region (build-region garden [x y] plant)]
                      (recur (conj regions region)
                             (into seen region)
                             [x' y']
                             (at garden [x' y']))))))))

(defn price [region]
  (let [area (count region)
        perimeter (reduce (fn [acc [x y]]
                            (+ acc (- 4 (count (filterv #(contains? region %) (possible-neighbours x y))))))
                          0 region)]
    (* area perimeter)))

;; idea for part 2, count the corners
;; but im tired and struggling to think of a way to do it that isnt stateful
;; so ill save it for another day

(->> (read-garden)
     (build-regions)
     (map price)
     (reduce +))