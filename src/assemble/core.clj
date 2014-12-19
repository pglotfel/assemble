(ns assemble.core
  (:require [manifold.deferred :as d]
            [assemble.graph :as g]
            [clojure.set :as st]
            [manifold.stream :as s])
  (:use [assemble.utils]))

;Multimethods to handle the parsing of each type of vertex

(def ^:private types [:unsafe :safe :source :cycle :sink :flow :alias])

(def ^:prviate type-pred #{:unsafe :safe})

(defn none 
  ([] (throw (IllegalArgumentException. "Can't call none!")))
  ([& xs] (throw (IllegalArgumentException. "Can't call none!"))))

(defn- type-priority
  [to-be-sorted] 
  (sort-by (fn [x] (.indexOf ^clojure.lang.PersistentVector types x)) to-be-sorted))

(defmulti parse-vertex-primary 
  (fn [env {:keys [type]} step con]
    (last type)))

;#### A vertex in a cycle ####

(defmethod parse-vertex-primary :cycle
  [env {:keys [title type generator transform]} step _]
  
  (case (some type-pred type)
    
    :safe 
    
    (assoc env title ((generator transform)))
    
    :unsafe
    
    (assoc env title (step))
    
    (do 
      (println "WARNING: No type detected for cycle " title " assuming unsafe")
      (assoc env title (step)))))

;#### A vertex with only outgoing edges ####

(defmethod parse-vertex-primary :source
  [env {:keys [title type generator transform]} step _]    
    
  (case (some type-pred type)
    
    :safe 
    
    (assoc env title ((generator transform)))
    
    :unsafe 
    
    (assoc env title (step))
    
    (do 
      (println "WARNING: No type detected for source " title " assuming unsafe")
      (assoc env title (step)))))

;#### A vertex with only incoming edges ####

(defmethod parse-vertex-primary :sink
  [env {:keys [title generator transform dependencies]} _ _] 
  (assoc env title (apply (generator transform) (map env dependencies))))

;#### A vertex with outgoing and incoming edges ####

(defmethod parse-vertex-primary :flow
  [env {:keys [title generator transform dependencies]} _ _]  
  (assoc env title (apply (generator transform) (map env dependencies))))

;#### A vertex that has had its output aliased ####

(defmethod parse-vertex-primary :alias 
  [env {:keys [title generator transform dependencies]} _ con] 
  (con (apply (generator transform) (map env dependencies)) (title env))
  env)

(defmethod parse-vertex-primary nil 
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

(defn make-graph
  [verticies]
  (reduce 
    (fn [m v]
      (assoc m (:title v) {:edges (set (:dependencies v))}))
    {}
    verticies))

(defn vertex
  
  "Pass in a title, dependencies, generator, transform, and, optionally, a group to create a vertex.
   In this case, a vertex is simply a hashmap with entries corresponding to the supplied arguments.  

  {:title :t
   :dependencies []
   :generator (fn [f])
   :transform (fn [x])
   :type []
   :group :verticies}

   Title is an identifier that uniquely defines the vertex. 
   
   Dependencies are the dependencies of the current vertex.  That is, titles of other vertices. 
 
   A generator is a function that returns a function describing how the output of the vertex is created. 

   The transform describes the functionality of the vertex over supplied inputs.  When assembled, the transform is
   passed as an argument to the generator function. 

   Group defines a specific group to which a vertex belongs.  This group can be required in the dependencies using a vector:
   [[:group]].  Additionally, the following keywords can be applied to group dependencies: 
   [[:group :only [:vertex]]] [:group :without [:vertex]]] to require or exclude certain vertices."

  [title dependencies generator transform & {:keys [group type] :or {group nil type []}}]  
  
{:title title
 :dependencies dependencies
 :generator generator 
 :transform transform
 :type type 
 :group group})
    
(defn assemble 
  [step con & verticies] 
  
  ;TODO: Implement some checks...
  
  (let [ts (map :title verticies)]
    (assert (= (count ts) (count (distinct ts))) "Each vertex must have a distinct name!"))
  
  (doseq [v verticies]
    (let [deps (:dependencies v)
          pred (set deps)]
      (assert (>= (count (filter (comp pred :title) verticies)) (count deps)) (str "Node " (:title v) "'s dependencies are not entirely defined within the supplied verticies."))))
  
  (let [compiler (fn [env v] (parse-vertex-primary env v step con))   
             
        ;#### Expand dependencies and infer types! ####
    
        [sccs with-deps] (let [groups (-> 
                                        
                                        (reduce (fn [m {:keys [title group]}] 
                                                  (if group 
                                                    (update-in m [group] (fn [x] (conj x title))) 
                                                    m)) 
                                                {} verticies)
                                        
                                        (assoc :all (mapv :title verticies)))
                        
                               deps-expanded (map (fn [v] (assoc v :dependencies (expand-dependencies groups (:dependencies v)))) verticies) 
                               
                               dependency-graph (make-graph deps-expanded)
                        
                               supply-graph (g/transpose dependency-graph)
                               
                               ;#### Retrieve all of the strongly connected components.  In this case, everything in a cycle. ####
                     
                               sccs (transduce (comp (remove (fn [vals] (if (= (count vals) 1) (let [val (vals 0)] (not (val (:edges (val supply-graph)))))))) 
                                                     
                                                  (remove empty?))
                                               
                                            conj 
                                               
                                            (g/dijkstras supply-graph))                                                                              
                        
                               pred (apply (comp set concat) sccs)]   
                        
                           [sccs (map (comp     
                                                     
                                        ;#### Make sure types are prioritized properly. #### 
                                     
                                        (fn [v] 
                                          (update-in v [:type] #(vec (type-priority %))))
                                        
                                     ;#### If not assigned by the user, assign safe/unsafe tags #### 
                                        
                                     (fn [v]                                         
                                       (let [type (:type v)]                                          
                                         (if (> (count type) 1)                                             
                                           v                                           
                                           (case (last type)        
                                                
                                             :sink                                             
                                             (update-in v [:type] #(conj % :safe))
                                                
                                             :source          
                                             (update-in v [:type] #(conj % :unsafe))
                                                
                                             :flow 
                                             (update-in v [:type] #(conj % :safe))
                                                
                                             :cycle
                                             ;#### Return vertex because we already handled cycle types earlier ####
                                             v   
                                                
                                             (do 
                                               (println "WARNING: unknown type in vertex " (:title v) " assuming unsafe")
                                               (update-in v [:type] #(conj % :unsafe)))))))
                                   
                                        ;#### Tag components in cycles... ####   
                       
                                        (fn [v]                            
                                          (if ((:title v) pred)
                                         ;#### Type for cycles is set! #### 
                                         (assoc v :type [:unsafe :cycle])
                                            v))
                           
                                        ;#### Infer vertex primary types! ####
                       
                                        (fn [v]   
                                          (let [title (:title v)                              
                                                graph-es (:edges (title supply-graph))                               
                                                transpose-es (:edges (title dependency-graph))]                            
                                            (if (empty? graph-es)
                                              (if (empty? transpose-es)
                                                (throw (IllegalArgumentException. (str "You have a vertex, " title ", with no dependencies and no dependents")))
                                                (update-in v [:type] #(conj % :sink)))
                                              (if (empty? transpose-es)
                                                (update-in v [:type] #(conj % :source))
                                                (update-in v [:type] #(conj % :flow))))))) 
                         
                                      deps-expanded)])                                 
             
        ;#### Get the sources and cycles for future reference! ####
    
        [sources cycles flow] (let [result (group-by #(last (:type %)) with-deps)]
                                [(:source result) (:cycle result) (apply concat (vals (dissoc result :source :cycle)))])
                   
        ;#### Compiler passes! ####
          
        env (reduce compiler {} (concat 
          
                                  sources 
                
                                  cycles 
                
                                  ;#### Do a topological sort on the remaining nodes #### 
                              
                                  (let [not-source|cycle (into {} (map (fn [v] [(:title v) v]) flow))]
                                    
                                    (transduce (comp (map not-source|cycle) (remove nil?)) conj (g/kahn-sort (g/transpose (make-graph (concat sources flow))))))                                
                                           
                                  (transduce (comp (map (fn [v] (update-in v [:type] 
                                                                        (fn [x]                                                                
                                                                             (if (= (first x) :unsafe)
                                                                               (assoc x (dec (count x)) :alias)
                                                                               x)))))
                                                   
                                                   (remove (fn [x] 
                                                          (not (= (last (:type x)) :alias)))))
                                             
                                             conj 
                                             
                                             (concat sources cycles))))]
       
    ;#### Next, I need to start all of the cycles.  Ooo, side effects! I currently do this by finding a feedback vertex set (FVS) of each cycle ####

    (doseq [v (| (mapcat (fn [group] (g/fvs (g/transpose (make-graph (filter (comp (set group) :title) cycles))))) sccs)
              
                 #(filter (comp (set %) :title) cycles) 
              
                 #(mapcat :dependencies %)
              
                 #(filter (comp (set %) :title) cycles))]    
      
      (step ((:title v) env) ((:transform v))))
    
    ;#### Associate streams back into the verticies! ####
    (map (fn [a b] (assoc a :output ((:title a) env) :type (:type b))) verticies with-deps)))

(defn output 
  "Retrieves the output of a given body" 
  [title & verticies]  
  (:output (find-first #(= (:title %) title) verticies)))

  
  
  
  
  
  
  
  
        
         
         
           
           
           
           
           
           
                 