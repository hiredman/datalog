(defproject datalog "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.5.0-master-SNAPSHOT"]
                 [org.apache.derby/derby "10.8.2.2"]
                 [org.clojure/java.jdbc "0.2.1"]]
  :profiles {:dev {:dependencies [[criterium "0.2.1"]]}}
  :plugins [[lein-swank "1.4.4"]])
