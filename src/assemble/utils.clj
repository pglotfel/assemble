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
  [^clojure.lang.PersistentVector coll key]
  (let [ val (.indexOf coll key)]
    (if (not= val -1) 
      val 
      -1)))

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

