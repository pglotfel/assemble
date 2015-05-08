(ns assemble.heap)

(defn parent 
  [index] 
  (int (/ (dec index) 2)))

(defn left 
  [index] 
  (inc (* 2 index)))

(defn right 
  [index] 
  (+ 2 (* 2 index)))

(defn max-heap-identity 
  [{:keys [heap]} index] 
  (assert (and (> (heap (parent index)) (heap (left index))) (> (heap (parent index)) (heap (right index)))) (str "At: " index " " (heap (parent index)) " " (heap (left index)) " " (heap (right index))))
  true)

;heap => {:size 3 :heap []}

(defn exchange 
  [{:keys [heap size] :as heap-map} a b] 
  (let [temp (heap a)]
    (-> 
      heap-map
      (assoc-in [:heap a] (heap b))
      (assoc-in [:heap b] temp))))

(defn- within-size? 
  [{:keys [heap size]} index]
    (if (< index size)
      (heap index)
      -1))

(defn max-heapify 
  ([heap-map] 
    (max-heapify heap-map 0))
  ([{:keys [^clojure.lang.PersistentVector heap size] :as heap-map} index]
    (let [l (left index)
          r (right index)          
          largest (.indexOf heap (max (within-size? heap-map l) (within-size? heap-map r) (heap index)))]
      (if (not= largest index)
        (max-heapify (exchange heap-map index largest) largest)
        heap-map))))
  
(defn reverse-range 
  [start end] 
  (loop [coll [] i start] 
    (if (>= i end)
      (recur (conj coll i) (dec i))
      coll)))

(defn build-max-heap 
  [heap]
  (reduce 
    (fn [current index] 
      (max-heapify current index))
    {:size (count heap) :heap heap}
    (reverse-range (int (/ (count heap) 2)) 0)))

(defn heap-max 
  [heap-map] 
  ((:heap heap-map) 0))

(defn heap-pop 
  [{:keys [heap size] :as heap-map}]
  (-> 
    (assoc heap-map :heap (subvec heap 1) :size (dec size))
    max-heapify))
  
 (time (def heap (build-max-heap (vec (range 10000)))))
  






