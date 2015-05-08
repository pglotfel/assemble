(ns assemble.graph
  (:require [clojure.set :as s])
  (:use [assemble.utils]))

(defn incoming? 
  "Returns true if the vertex has incoming edges."
  [graph vertex] 
  (if vertex
    (some (comp vertex :edges) (vals graph))
    true))

(defn outgoing? 
  "Returns true if the vertex has outgoing edges."
  [graph vertex] 
  (> (count (:edges (graph vertex))) 0))

(defn transpose
  "Returns the transpose of a graph (e.g., directions of all edges reversed)."
  [graph] 
  (let [verticies (keys graph)]
    (reduce 
      (fn [m v]
        (reduce 
          (fn [m u]
            (update-in m [u :edges] (fn [x] (conj x v))))
          m 
          (:edges (v graph))))
      (reduce (fn [m v] (assoc m v {:edges #{}})) {} verticies)
      verticies)))

(defn outgoing 
  "Returns the number of outgoing edges of a vertex."
  [graph vertex] 
  (:edges (vertex graph)))

(defn incoming 
  "Returns the number of incoming edges of a vertex."
  [graph vertex] 
  (reduce-kv 
    (fn [coll k v] 
      (if (contains? (:edges v) vertex)
        (conj coll k)
        coll))
    []
    graph))

(defn- sink? 
  "Returns true if the vertex only has incoming edges."
  [graph vertex] 
  (and (>= (count (incoming graph vertex)) 0) (<= (count (outgoing graph vertex)) 0)))

(defn- source? 
  "Returns true if the vertex only has outgoing edges."
  [graph vertex] 
  (and (<= (count (incoming graph vertex)) 0) (> (count (outgoing graph vertex)) 0)))

(defn- degree 
  "Returns the number of incoming edges and the number of outgoing edges."
  [graph vertex] 
  (+ (count (outgoing graph vertex)) (count (incoming graph vertex))))

(defn- highest-degree 
  "Given a graph, returns the vertex with the highest degree."
  [graph]
  (let [result (volatile! nil)]
    (reduce-kv 
      (fn [max k v]         
        (let [deg (degree graph k)]
          (if (> deg max) 
            (do
              (vreset! result k)
              deg)
            max)))
      -1
      graph)
    @result))

(defn- remove-vertex 
  "Removes a vertex from the graph, updating the edges accordingly."
  [graph & vertices] 
  (let [graph' (apply dissoc graph vertices)] 
    (reduce
        (fn [m k]          
          (update-in m [k] (fn [v] (update-in v [:edges] (fn [es] (apply disj es vertices))))))
        graph'
        (keys graph')))) 

(defn prune 
  "Takes a predicate, removing all verticies for which the predicate returns true.  The predicate will be called with the graph and vertex as arguments.  Returns false if none of the predicates returned true."
   [graph pred] 
   (let [ks (keys graph)     
         to-remove (reduce 
                     (fn [coll k] 
                       (if (pred graph k)
                         (conj coll k) 
                         coll))
                     #{}
                     ks)]   
     (if (empty? to-remove)
       false
       (apply remove-vertex graph to-remove))))

(defn- try-until 
  [preds & inputs] 
  (reduce
    (fn [result pred] 
      (if (apply pred inputs)
        (reduced true)
        result))
    false 
    preds))

(defn continuously-remove 
  [graph & preds] 
  (let [helper (fn this 
                 [g] 
                 (let [result (prune g (fn [g v] (try-until preds g v)))]
                   (if result
                     #(this result)
                     g)))]
    (trampoline helper graph)))

(defn remove-sources 
  "Recursively removes sources in a graph until none remain"
  [graph]
  (continuously-remove graph source?))

(defn remove-sinks 
  "Recursively removes sinks in a graph until none remain"
  [graph] 
  (continuously-remove graph sink?))

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
        (recur graph' (conj l n) (concat (rest s) (remove #(incoming? graph' %) m)))))))

;Change this into a feedback arc set implementation.  Will simplify initial conditions injection

(defn fvs 
  "Calculates an fvs given a graph by implementing a slightly altered version of the algorithm described in http://www.sciencedirect.com/science/article/pii/002001909390079O"
  [graph]
  (let [helper (fn this [coll graph] 
                 (let [graph' (continuously-remove graph sink? source?)]     
                   (if (empty? graph')
                     coll
                     (let [vertex (highest-degree graph')]                                     
                       #(this (conj coll vertex) (remove-vertex graph' vertex))))))]
  
    (trampoline helper [] graph)))

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
  
                
  











