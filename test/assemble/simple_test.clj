(ns assemble.simple-test
  (:require [assemble.core :as a]))


(time 
  (a/assemble
  
    a/none
  
    a/none

    (a/vertex :a [] (fn [f] (fn [] (f))) (fn [] 1) :type [:safe])
        
    (a/vertex :b [:a] (fn [f] (fn [x] (f x))) (fn [x] (inc x)))))




