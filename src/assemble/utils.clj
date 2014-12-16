(ns assemble.utils
  (:use [clojure.java.shell :only [sh]]))

(defn find-first 
  [pred coll] 
  (reduce 
    (fn [found? val] 
      (if (pred val)
        (reduced val)
        found?))
    false 
    coll))

(defn containsv? 
  [coll key]
  (reduce 
    (fn [contains? v]
      (if (= v key)
        (reduced true)
        contains?))
    false 
    coll))

(defn | 
  [init & fns] 
  ((apply comp (reverse fns)) init))

(defn time-now 
  []
  "Returns the current time"
  (. System (nanoTime)))

(defn time-passed
  [start-time]
  "Returns the time passed"
  (/ (double (- (. System (nanoTime)) start-time)) 1000000.0))

