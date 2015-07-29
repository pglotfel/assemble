(ns assemble.graph
  (:require [clojure.set :as s])
  (:use [assemble.utils]))

;convert these functions to the faster forms that I already made 

;Change the format to {:vertex {:edges #{} :pi #{}}}

(defn incoming? 
  "Returns true if the vertex has incoming edges."
  [graph vertex] 
  (> (count (:pi (vertex graph))) 0))

(defn outgoing? 
  "Returns true if the vertex has outgoing edges."
  [graph vertex] 
  (> (count (:edges (graph vertex))) 0))

(defn transpose
  "Returns the transpose of a graph (e.g., directions of all edges reversed)."
  [graph] 
  (reduce-kv
    (fn [g k v]   
      (-> 
        (assoc-in g [k :edges] (:pi v))
        (assoc-in [k :pi] (:edges v))))
    {}
    graph))

(defn outgoing 
  "Returns the number of outgoing edges of a vertex."
  [graph vertex] 
  (:edges (vertex graph)))

(defn incoming 
  "Returns the number of incoming edges of a vertex."
  [graph vertex] 
  (:pi (vertex graph)))

(defn- sink? 
  "Returns true if the vertex only has incoming edges."
  [graph vertex] 
  (and (>= (count (incoming graph vertex)) 0) (<= (count (outgoing graph vertex)) 0)))

(defn- source? 
  "Returns true if the vertex only has outgoing edges."
  [graph vertex] 
  (and (<= (count (incoming graph vertex)) 0) (> (count (outgoing graph vertex)) 0)))

(defn sources 
  [graph] 
  (reduce 
    (fn [coll u] 
      (if (source? graph u)
        (conj coll u)
        coll))
    []
    (keys graph)))

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

(defn add-edge 
  [graph u v]
  (-> 
    (update-in graph [u :edges] (fn [x] (conj x v)))
    (update-in [v :pi] (fn [x] (conj x u)))))

(defn remove-edge 
  "Removes an edges, updating edges and pis accordingly."
  [graph u v]
  (-> 
    (update-in graph [u :edges] (fn [x] (disj x v)))
    (update-in [v :pi] (fn [x] (disj x u)))))

(defn remove-edges 
  [graph u & vs] 
  (reduce (fn [g v] (remove-edge g u v)) graph vs))

(defn remove-vertex
  [graph v] 
  (|
    (reduce (fn [g u] (remove-edge g v u)) graph (:edges (v graph)))
    #(reduce (fn [g u] (remove-edge g u v)) % (:pi (v graph)))
    #(dissoc % v)))

(defn remove-vertices
  [graph & vs] 
  (reduce remove-vertex graph vs))

(defn remove-source 
  [graph vertex] 
  "Removes a source from the graph and recursively ensures that none of its descendents are sources."
  (if (source? graph vertex)
    (| 
      (remove-vertex graph vertex)
       
      #(reduce 
          remove-source
          % 
          (:edges (vertex graph))))
    
    graph))

(defn remove-sink
  "Removes a sink from the graph and recursively ensures that none of its predecessors are sources."
  [graph vertex] 
  (if (sink? graph vertex)
    (| 
      (remove-vertex graph vertex)
      
      #(reduce 
          remove-sink
          % 
          (:pi (vertex graph))))
    
    graph))

(defn remove-sources
  "Recursively removes all the sources in a graph"
  [graph] 
  (reduce 
    remove-source
    graph 
    (keys graph)))
    
(defn remove-sinks
  "Recursively removes all the sinks in a graph"
  [graph] 
  (reduce 
    remove-sink
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
            graph' (apply remove-edges graph n m)]     
        (recur graph' (conj l n) (concat (rest s) (remove #(incoming? graph' %) m)))))))

;Change this into a feedback arc set implementation.  Will simplify initial conditions injection

(defn fas
  "Calculates an fas given a graph by implementing a slightly altered version of the algorithm described in http://www.sciencedirect.com/science/article/pii/002001909390079O"
  [graph]
  (let [helper (fn this [coll graph] 
                 (let [graph' (-> 
                                
                               (remove-sinks graph)
                                
                               remove-sources)]     
           
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
  
               










