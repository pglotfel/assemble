(ns assemble.rand-test
  (:require [assemble.core :as a]
            [assemble.graph :as g]))

;#### NOT WORKING AT ALL! ####

(defn take-rand 
  [coll]
  (take (rand-int (inc (count coll))) coll))

(defn gen-title
 [n]
 (mapv (comp keyword str) (range n)))

(def ^:private source-gen (fn [f] (fn [] (f))))
(def ^:private other-gen (fn [f] (fn [& xs] (apply f xs))))
(def ^:private types [:source :other])
(def ^:private transform (fn
                           ([] 1)
                           ([& xs] (apply + xs))))

(defn step 
  ([] 1)
  ([x] x)
  ([x input] (+ x input)))

(defn connect 
  [x y])

(def system 
  [(a/vertex :a [] source-gen transform) 
   (a/vertex :b [] source-gen transform)
   (a/vertex :c [:a :b] other-gen transform)])

(apply a/assemble step connect system)

;(defn gen-generator 
;  [type]  
;  (case type   
;    :source 
;    source-gen     
;    other-gen))
;
;(defn gen-vertex
; [title & titles] 
; (let [type (types (rand-int 2))
;       gen (gen-generator type)
;       deps (if (= type :source)
;              []
;              (take-rand titles))]
;   (a/vertex title (vec (remove #(= title %) deps)) gen transform)))
;
;(defn gen-verticies 
;  [n] 
;  (let [titles (gen-title n)]
;    (map #(apply gen-vertex % titles) titles)))
;
;(defn remove-empty 
;  [& verticies] 
;  (let [graph (a/make-graph verticies)    
;        
;        sccs (->>                                     
;               (g/dijkstras graph)                                                                                                                                            
;               (remove
;                (fn [vals]
;                  (if (= (count vals) 1)
;                    (let [val (vals 0)]
;                      (not (val (:edges (val graph))))))))              
;               (remove empty?))
;        
;        verticies' (remove (comp (set (flatten sccs)) :title) verticies)
;        
;        graph (a/make-graph verticies')
;        
;        verticies' (remove (comp (set (filter (fn [x] (not (or (g/outgoing? graph x) (g/incoming? graph x)))) (keys graph))) :title) verticies')
;        
;        titles' (map :title verticies')]   
;    
;    (println sccs)
;    (println (a/make-graph verticies'))
;    
;    (println (reduce 
;               (fn [coll v]
;                 (conj coll (update-in v [:dependencies] (fn [x] (transduce (comp (map (set (:dependencies x))) (remove nil?)) conj titles')))))
;               []
;               verticies'))))
;
;(defn gen-test 
;  [n]
;  (let [verticies (apply remove-empty (gen-verticies n))]
;    (apply a/assemble verticies)))












