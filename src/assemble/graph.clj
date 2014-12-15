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

(defn- dfs-visit 
  ([graph node time] (dfs-visit graph node time nil))
  ([graph node time tree] 
    (if tree 
      (conj! tree node))   
    (->       
      (reduce (fn [graph vertex]                 
                (if-not (or (:start-time (vertex graph)) (:end-time (vertex graph)))           
                  (dfs-visit graph vertex time tree)              
                  graph))       
              (assoc-in graph [node :start-time] (swap! time inc))            
              (:edges (node graph)))     
      (assoc-in [node :end-time] (swap! time inc)))))

(defn dfs  
  [graph] 
  (let [time (atom 0)]
    (reduce (fn [graph vertex]            
              (if-not (or (:start-time (vertex graph)) (:end-time (vertex graph)))        
                (dfs-visit graph vertex time nil)             
                graph))            
            graph           
            (keys graph))))

(defn dfs-tree  
  [graph & {:keys [order] :or {order (vec (keys graph))}}] 
  (let [time (atom 0)      
        trees (vec (repeatedly (count order) (comp transient vector)))]   
    (reduce-kv (fn [graph cardinal vertex]            
                 (if-not (or (:start-time (vertex graph)) (:end-time (vertex graph)))            
                   (dfs-visit graph vertex time (trees cardinal))           
                   graph))            
               graph            
               order)    
    (map persistent! trees)))
              
(defn- tally-dfs   
  [dfs-result]   
  (->>   
    (map (fn [[k v]]            
           [k (:end-time v)])         
         dfs-result)    
    (sort-by second)   
    (map first)    
    reverse))
  
(defn strongly-connected-components  
  "Finds strongly connected components using an implmentation of Kosaraju's algorithm http://en.wikipedia.org/wiki/Kosaraju%27s_algorithm"
  [graph transpose-graph]  
  (->>  
    (->   
      (dfs graph)  
      tally-dfs     
      vec)
    (dfs-tree transpose-graph :order)))

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

;Set the preorder number of v to C, and increment C.
;Push v onto S and also onto P.
;For each edge from v to a neighboring vertex w:
;If the preorder number of w has not yet been assigned, recursively search w;
;Otherwise, if w has not yet been assigned to a strongly connected component:
;Repeatedly pop vertices from P until the top element of P has a preorder number less than or equal to the preorder number of w.
;If v is the top element of P:
;Pop vertices from S until v has been popped, and assign the popped vertices to a new component.
;Pop v from P.

(defn- containsv? 
  [coll key]
  (reduce 
    (fn [contains? v]
      (if (= v key)
        (reduced true)
        contains?))
    false 
    coll))

;Clojurefy this if possible!

(defn dijkstras
  "Implementation of Dijkstra's strongly connected component algorithm described in http://en.wikipedia.org/wiki/Path-based_strong_component_algorithm"
  ([graph]    
    (let [graph (atom graph) 
          c (atom 0)
          s (atom []) 
          p (atom [])
          components (atom [])]
      (doseq [v (keys @graph)]
        (if-not (:preorder (v @graph))
          (dijkstras graph v c s p components)))
      @components))
  ([graph v c s p components] 
      
      (let [edges (:edges (v @graph))]      
            
        (swap! graph update-in [v] (fn [x] (assoc x :preorder @c)))
        (swap! c inc)      
        (swap! s conj v)
        (swap! p conj v)
      
        (doseq [w edges]   
          (if (:preorder (w @graph))
            (do
              (when-not (some #(containsv? % w) @components)
                (loop [p' @p]
                  (when (> (:preorder ((last p') @graph)) (:preorder (w @graph)))
                    (do 
                      (swap! p pop)
                      (recur @p))))))
            (dijkstras graph w c s p components)))

        (when (= (last @p) v)
          (loop [s' @s cs []]
            (if (= (last s') v)
              (do 
                (swap! s pop)
                (swap! components conj (conj cs v)))
              (do 
                (swap! s pop)
                (recur @s (conj cs (last s'))))))
          (swap! p pop)))))
  
                
  











