(ns assemble.simple-test
  (:require [assemble.core :as a]
            [assemble.graph :as g]
            [criterium.core :as c]))

(defn generate-system 
  [n] 
  
  (let [generator (fn [f] (fn [x] (f x)))
        
        transform inc
        
        idx (volatile! 0)]
  
  (conj 
    
    (repeatedly 
      
      n 
      
      (fn [] 
            
        (let [prev @idx]
            
          (a/vertex (keyword (str (vswap! idx inc))) [(keyword (str prev))] generator transform)))) 
        
        
    (a/vertex :0 [] (fn [f] (fn [] (f))) (fn [] 1) :type [:safe]))))

(defn test-system 
  [] 
  (let [sys (generate-system 1000)]
    (apply a/assemble a/none a/none sys)))

(def system (generate-system 1000))

(def graph (a/make-graph system))
