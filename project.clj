(defproject brambling "0.0.3"
  :description "Datomic migration toolkit"
  :url "http://github.com/bitemyapp/brambling/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repl-options {:port 5445}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clj-time "0.6.0"]
                 ;; Install Datomic Pro or exclude and replace with datomic-free if needs be.
                 [com.datomic/datomic-pro "0.8.4254"]]
  :test-selectors {:default (complement :incremental)})
