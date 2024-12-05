(ns day5.day5)

(defn read-orderings []
  (->> (slurp "inputs/day5.txt") ; slurp 😋   
       (re-seq #"\d+\|\d+") ; only get the ordering data
       (map #(map Integer/parseInt (re-seq #"\d+" %))) ; split the ordering data into the ordering pairs
       (reduce (fn [orderings [x y]]
                 (update-in orderings [y] (fnil conj #{}) x)) ; map of all pages that must come before y
               {})))

(defn read-updates []
  (->> (slurp "inputs/day5.txt") ; slurp 😋   
       (re-seq #"(\d+,)+\d+") ; only get the lists of pages
       (map first)
       (map #(map Integer/parseInt (re-seq #"\d+" %))))) ; turn into lists of ints

(defn is-valid-update? [orderings update]
  (->> update
       (reduce (fn [[invalid? invalid-pages] page]
                 (if invalid? (reduced [true invalid-pages]) ; if invalid, its reduced
                     [(contains? invalid-pages page) ; its invalid if the page is in invalid-pages
                      (apply conj invalid-pages (orderings page))])) ; add pages that must come before page to invalid-pages
               [false #{}])
       (first)
       (not))) ; negate invalid?

;; part 1
(let [orderings (read-orderings)]
  (->> (read-updates)
       (filter #(is-valid-update? orderings %)) ; only take valid updates
       (map #(nth % (quot (count %) 2))) ; median value
       (reduce +))) ; sum

;; part 2
(let [orderings (read-orderings)]
  (->> (read-updates)
       (filter #(not (is-valid-update? orderings %))) ; only take invalid updates
       (map #(sort (fn [a b] (contains? (orderings b) a)) %)) ; sort -> a < b if orderings[b] contains a
       (map #(nth % (quot (count %) 2))) ; median value
       (reduce +))) ; sum
