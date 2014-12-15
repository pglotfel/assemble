(ns assemble.graph
  (:require [clojure.set :as s]))

(defn- incoming? 
  [graph vertex] 
  (if vertex
    (some (comp vertex :edges) (vals graph))
    true))

(defn transpose 
  [graph]  
  (let [nodes (keys graph)] 
    (apply merge (map (fn [node]        
                         {node {:edges (reduce (fn [edges k]                                
                                                 (if (node (:edges (k graph)))                             
                                                   (conj edges k)                                          
                                                   edges))                            
                                               #{} nodes)}})                    
                      nodes))))

(defn- outgoing 
  [graph vertex] 
  (:edges (vertex graph)))

(defn- incoming 
  [graph vertex] 
  (reduce-kv 
    (fn [coll k v] 
      (if (contains? (:edges v) vertex)
        (conj coll k)
        coll))
    []
    graph))

(defn- sink? 
  [graph vertex] 
  (and (>= (count (incoming graph vertex)) 0) (<= (count (outgoing graph vertex)) 0)))

(defn- source? 
  [graph vertex] 
  (and (<= (count (incoming graph vertex)) 0) (> (count (outgoing graph vertex)) 0)))

(defn- degree 
  [graph vertex] 
  (+ (count (outgoing graph vertex)) (count (incoming graph vertex))))

(defn- highest-degree 
  [graph]
  (let [result (atom nil)]
    (reduce-kv 
      (fn [max k v]         
        (let [deg (degree graph k)]
          (if (> deg max) 
            (do
              (reset! result k)
              deg)
            max)))
      -1
      graph)
    @result))

(defn- remove-vertex 
  [graph vertex] 
  (let [graph' (dissoc graph vertex)] 
    (reduce
        (fn [m k]          
          (update-in m [k] (fn [v] (update-in v [:edges] (fn [es] (disj es vertex))))))
        graph'
        (keys graph')))) 

(defn prune 
   [graph pred] 
   (let [ks (keys graph)     
         to-remove (reduce 
                     (fn [coll k] 
                       (if (pred graph k)
                         (conj coll k) 
                         coll))
                     #{}
                     ks)]   
     (reduce 
       remove-vertex
       graph 
       to-remove)))


(defn remove-sinks 
  "Recursively removes sinks in a graph until none remain"
  [graph] 
  (let [helper (fn this 
                 [g]
                 (let [result (prune g (fn [g v] (sink? g v)))]
                   (if (= result g)
                     g 
                     #(this result))))]
    (trampoline helper graph)))

(defn remove-sources 
  "Recursively removes sources in a graph until none remain"
  [graph]
  (let [helper (fn this 
                 [g]
                 (let [result (prune g (fn [g v] (source? g v)))]
                   (if (= result g)
                     g 
                     #(this result))))]
    (trampoline helper graph)))

(defn to-graph 
  [graph f] 
  (reduce
        (fn [m k]          
          (update-in m [k] f k))
        graph
        (keys graph)))

(defn kahn-sort  
  "Topologically sorts a graph using the kahn algorithm, where graph is a DAG graph in the style {:vertex {:edges #{...}}}, l is the resulting order, and s is the nodes to be traversed." 
  ([graph] (kahn-sort graph [] (remove #(incoming? graph %) (keys graph))))
  ([graph l s]
    (if (empty? s)
      (if (every? (comp empty? :edges) (vals graph)) 
        l)  
      (let [n (first s)            
            m (:edges (n graph))         
            graph' (reduce (fn [g v] (update-in g [n :edges] disj v)) graph m)]     
        (recur graph' (conj l n) (concat (rest s) (filter (comp #(not (incoming? graph' %)) m) (keys graph'))))))))

(defn fvs 
  "Calculates an fvs given a graph by implementing a slightly altered version of the algorithm described in http://www.sciencedirect.com/science/article/pii/002001909390079O"
  [graph]
  (let [helper (fn this [coll graph] 
                 (let [graph++ (-> 
                                 (prune graph (fn [g v] (:marked (g v))))
                                 remove-sinks
                                 remove-sources)]     
                   (if (empty? graph++)
                     coll
                     (let [vertex (highest-degree graph++)]                                     
                       #(this (conj coll vertex) (-> 
                                                   
                                                   (to-graph graph++ (fn [v k] 
                                                                       (if (and (contains? (:edges v) vertex) (contains? (:edges (vertex graph)) k))
                                                                         (assoc v :marked true)
                                                                         v)))
                                                   
                                                   (remove-vertex vertex)))))))
        
        result (trampoline helper [] graph)]
    
    ;Ensure that removal of vertices makes a DAG
  
    (if (kahn-sort (reduce remove-vertex graph result))
      result
      (do 
        (println "WARNING: cycles may not start correctly!")
        result))))

(defn- containsv? 
  [coll key]
  (reduce 
    (fn [contains? v]
      (if (= v key)
        (reduced true)
        contains?))
    false 
    coll))

;Make this implementation more functional

(defn dijkstras
  "Implementation of Dijkstra's strongly connected component algorithm described in http://en.wikipedia.org/wiki/Path-based_strong_component_algorithm"
  ([graph]    
    (let [graph (volatile! graph) 
          c (volatile! 0)
          s (volatile! []) 
          p (volatile! [])
          components (volatile! [])]
      (doseq [v (keys @graph)]
        (if-not (:preorder (v @graph))
          (dijkstras graph v c s p components)))
      @components))
  ([graph v c s p components] 
      
      (let [edges (:edges (v @graph))]      
            
        (vswap! graph update-in [v] (fn [x] (assoc x :preorder @c)))
        (vswap! c inc)      
        (vswap! s conj v)
        (vswap! p conj v)
      
        (doseq [w edges]   
          (if (:preorder (w @graph))
            (do
              (when-not (some #(containsv? % w) @components)
                (loop [p' @p]
                  (when (> (:preorder ((last p') @graph)) (:preorder (w @graph)))
                    (do 
                      (vswap! p pop)
                      (recur @p))))))
            (dijkstras graph w c s p components)))

        (when (= (last @p) v)
          (loop [s' @s cs []]
            (if (= (last s') v)
              (do 
                (vswap! s pop)
                (vswap! components conj (conj cs v)))
              (do 
                (vswap! s pop)
                (recur @s (conj cs (last s'))))))
          (vswap! p pop)))))
  
                
  











