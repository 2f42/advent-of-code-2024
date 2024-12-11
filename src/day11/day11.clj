(ns day11.day11)

(defn read-stones []
  (->> (slurp "inputs/day11.txt") ; slurp 😋
       (re-seq #"\d+")
       (mapv parse-long)
       (reduce (fn [m k]
                 (update m k (fnil inc 0)))
               {})))

(defn split-number [n]
  (let [digits (str n)
        half (quot (count digits) 2)]
    [(parse-long (subs digits 0 half))
     (parse-long (subs digits half))]))

(defn blink [stones]
  (->> stones
       (reduce-kv (fn [m k v]
                    (cond
                      (zero? k) (assoc! m 1 v)
                      (even? (count (str k))) (let [[a b] (split-number k)]
                                                (-> m
                                                    (assoc! a ((fnil #(+ v %) 0) (get m a)))
                                                    (assoc! b ((fnil #(+ v %) 0) (get m b)))))
                      :else (assoc! m (* 2024 k) ((fnil #(+ v %) 0) (get m (* 2024 k))))))
                  (transient {}))
       persistent!))

(defn blinks [n stones]
  (loop [counter 0
         stones' stones]
    (if (= counter n) stones'
        (recur (inc counter) (blink stones')))))

(->> (read-stones)
     (blinks 75)
     (reduce-kv (fn [acc _ v]
                  (+ acc v)) 0))