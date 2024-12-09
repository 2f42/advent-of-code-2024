(ns day9.day9)

(defn read-filesystem []
  (->> (slurp "inputs/day9.txt")
       (re-seq #"\d")
       (map Integer/parseInt)
       (partition 2 2 [0])
       (reduce (fn [{:keys [t id]} [size free]]
                 {:t (conj! t {:size size :free free :id id})
                  :id (inc id)})
               {:t (transient []) :id 0})
       (:t)
       (persistent!)))

(defn filesystem->list [filesystem]
  (->> filesystem
       (reduce (fn [t {:keys [id size free]}]
                 (conj! (conj! t (take size (cycle [id])))
                        (take free (cycle [nil]))))
               (transient []))
       (persistent!)
       (flatten)))

(defn checksum [filesystem]
  (:acc (reduce (fn [{:keys [acc i]} x]
                  {:acc (+ ((fnil * 0) x i) acc)
                   :i (inc i)})
                {:acc 0 :i 0} filesystem)))

;; part 1
(defn compact-filesystem [filesystem]
  (let [file-list (filesystem->list filesystem)
        file-count (count (remove nil? file-list))]
    (->> file-list
         (reduce (fn [{:keys [result files]} id]
                   (if (>= (count result) file-count) (reduced {:result result})
                       (case id
                         nil {:result (conj! result (last files))
                              :files (butlast files)}
                         {:result (conj! result id)
                          :files files})))
                 {:result (transient []) :files (remove nil? file-list)})
         (:result)
         (persistent!))))

;; part 2
(defn fit [files file]
  (-> (reduce (fn [{:keys [done? order]} {:keys [id size free]}]
                (cond
                  done? {:done? true
                         :order (conj! order {:id id :size size :free free})}
                  (<= (:size file) free) {:done? true
                                          :order (conj! (conj! order {:id id :size size :free 0})
                                                        {:id (:id file)
                                                         :size (:size file)
                                                         :free (- free (:size file))})}
                  :else {:done? false
                         :order (conj! order {:id id :size size :free free})}))
              {:done? false
               :order (transient [])} files)
      (update :order persistent!)))

(defn update-last [filesystem {:keys [size free]}]
  (conj (apply vector (butlast filesystem))
        (update (last filesystem) :free #(+ % size free))))

(defn fit-id [files id]
  (let [first-part (take-while #(not= id (:id %)) files)
        last-part (drop-while #(not= id (:id %)) files)
        file (first last-part)
        attempt-fit (fit first-part file)]
    (if (:done? attempt-fit) (concat (update-last (:order attempt-fit) file) (rest last-part))
        (concat first-part last-part))))

(defn fit-last [filesystem]
  (reduce fit-id filesystem (reverse (range (count filesystem)))))

(->> (read-filesystem)
     (fit-last)
     (filesystem->list)
     (checksum))
