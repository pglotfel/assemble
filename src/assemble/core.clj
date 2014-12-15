(ns assemble.core
  (:require [manifold.deferred :as d]
            [assemble.graph :as g]
            [clojure.set :as st]
            [manifold.stream :as s])
  (:use [watershed.utils]))

(set! *warn-on-reflection* true)

;Multimethods to handle the parsing of each type of vertex

(defmulti parse-vertex 
  (fn [env vertex step con]
    (:type vertex)))

(defmethod parse-vertex :cyclic
  [env {:keys [title _ _]} step _]
  (assoc env title (step)))

(defmethod parse-vertex :source
  [env {:keys [title _ _]} step _]
  (assoc env title (step)))

(defmethod parse-vertex :sink
  [env {:keys [title generator transform dependencies]} _ _] 
  (assoc env title (apply (generator transform) (map env dependencies))))

(defmethod parse-vertex :flow
  [env {:keys [title generator transform dependencies]} _ _]  
  (assoc env title (apply (generator transform) (map env dependencies))))

(defmethod parse-vertex :aliased 
  [env {:keys [title generator transform dependencies]} _ con] 
  (con (apply (generator transform) (map env dependencies)) (title env))
  env)

(defmethod parse-vertex nil 
  [env _ _ _ ]
  env)

(defn- expand-dependencies
  "Expands the dependencies of a given vertex given the groups and its dependencies"
  [groups dependencies]
  (vec (flatten (map (fn [dependency]                
                       (if (vector? dependency)                      
                         (let [[id op args] dependency] 
                           (case op                            
                             :only (vec (filter (set args) (id groups)))
                             :without (vec (remove (set args) (id groups)))
                             (id groups)))                                                  
                         dependency))               
                     dependencies))))

(defn- dependents
  [veticies t] 
  "Returns a set containing the dependents of a given vertex"
  (reduce      
    (fn [coll {:keys [title dependencies]}]              
      (if (some #{t} dependencies) (conj coll title) coll)) #{} veticies))

(defn- make-graph 
  "Makes a graph out of the given verticies"
  [veticies] 
  (reduce (fn [m {:keys [title]}]                      
            (assoc m title {:edges (dependents veticies title)}))                     
          {} veticies))

(def ^{:private true} o {:title nil :dependencies nil :generator nil :transform nil :group nil})

(defn vertex
  ([title dependencies generator transform] (vertex title dependencies generator transform nil))
  ([title dependencies generator transform group]
    (assoc o :title title :dependencies dependencies :generator generator :transform transform :group group)))
    
(defn assemble 
  [step con & veticies] 
  
  ;Implement some checks...
 
  (let [ts (map :title veticies)]
    (assert (= (count ts) (count (distinct ts))) "Each vertex must have a distinct name!"))
  
  (let [compiler (fn [env o] (parse-vertex env o step con))   
             
        ;#### Expand dependencies and infer types! ####
       
        [sccs with-deps] (let [groups (-> 
                                        
                                        (reduce (fn [m {:keys [title group]}] 
                                                  (if group 
                                                    (update-in m [group] (fn [x] (conj x title))) 
                                                    m)) 
                                                {} veticies)
                                        
                                        (assoc :all (mapv :title veticies)))
                        
                               deps-expanded (map (fn [o] (assoc o :dependencies (expand-dependencies groups (:dependencies o)))) veticies) 
                        
                               graph (make-graph deps-expanded)
                        
                               transpose (g/transpose graph)
                               
                               ;Retrieve all of the strongly connected components.  In this case, everything in a cycle.
                        
                               sccs (->>
                                      
                                      (g/strongly-connected-components graph (g/transpose graph))                                                                      
                                                                       
                                      (remove
                                        (fn [vals]
                                          (if (= (count vals) 1)
                                            (let [val (vals 0)]
                                            (not (val (:edges (val graph))))))))
                                      
                                      (remove empty?))                                                           
                        
                               pred (apply (comp set concat) sccs)]   
                    
                           [sccs (map (comp                           
                                   
                                        ;#### Tag components in cycles... ####   
                          
                                        (fn [o]                            
                                          (if ((:title o) pred)
                                            (assoc o :type :cyclic)
                                            o))
                           
                                        ;#### Infer vertex types! ####
                          
                                        (fn [o]   
                                          (if (:type o)
                                            o
                                            (let [title (:title o)                                
                                                  graph-es (:edges (title graph))                               
                                                  transpose-es (:edges (title transpose))]                            
                                              (if (empty? graph-es)
                                                (if (empty? transpose-es)
                                                  (throw (IllegalArgumentException. (str "You have a vertex, " title ", with no dependencies and no dependents")))
                                                  (assoc o :type :sink))
                                                (if (empty? transpose-es)
                                                  (assoc o :type :source) 
                                                  (assoc o :type :flow))))))) 
                         
                                      deps-expanded)])                                 
             
        ;#### Get the sources and cycles for future reference! ####
       
        sources (filter #(= (:type %) :source) with-deps)
        
        cycles (filter #(= (:type %) :cyclic) with-deps)
                   
        ;#### Compiler passes! ####
             
        env (reduce compiler {} (concat 
          
                                  sources 
                
                                  cycles 
                
                                  ;#### Do a topological sort on the remaining nodes #### 
                                 
                                  (let [non-cyclic (into {} (map (fn [o] [(:title o) o]) 
                                                                 (remove (fn [o]                 
                                                                           (let [type (:type o)]
                                                                             (or (= type :cyclic) (= type :source) (= type :dam))))                  
                                                                           with-deps)))]
                                          
                                    (->> 
                                            
                                      (make-graph (vals non-cyclic))
                                            
                                      g/kahn-sort
                                            
                                      (map non-cyclic)))
                                           
                                  (map (fn [o] (assoc o :type :aliased)) (concat sources cycles))))]
       
    ;#### Next, I need to start all of the cycles.  Ooo, side effects! I currently do this by finding a feedback vertex set (FVS) of each cycle ####
   
    (doseq [o (| (mapcat (fn [group] (g/fvs (make-graph (filter (comp (set group) :title) cycles)))) sccs)
              
                 #(filter (comp (set %) :title) cycles) 
              
                 #(mapcat :dependencies %)
              
                 #(filter (comp (set %) :title) cycles))]                     
      
      (step ((:title o) env) ((:transform o))))
    
    ;#### Associate streams back into the veticies! ####
    (map (fn [a b] (assoc a :output ((:title a) env) :type (:type b))) veticies with-deps)))

(defn output 
  "Retrieves the output of a given body" 
  [title & veticies]  
  (:output (find-first #(= (:title %) title) veticies)))

  
  
  
  
  
  
  
  
        
         
         
           
           
           
           
           
           
                 