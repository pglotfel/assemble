(defproject assemble "0.1.0-SNAPSHOT"
  :description "A library for assembling graphs!"
  :url "http://github.com/pglotfel/assemble"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :global-vars {*warn-on-reflection* true}
  :dependencies [[org.clojure/clojure "1.7.0-alpha4"]  
[org.clojure/data.generators "0.1.2"]
                 [manifold "0.1.0-beta3"]
                 [criterium "0.4.3"]
                 [org.clojure/data.int-map "0.1.0"]])

