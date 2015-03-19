(defproject clj-predict "0.0.1-SNAPSHOT"
  :description "Library for satellite propagation and other space maths."
  :url "https://github.com/david-rc-dayton/clj-predict"
  :license {:name "The MIT License (MIT)"
            :url "http://opensource.org/licenses/MIT"
            :distribution :repo}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [uk.me.g4dpz/predict4java "1.1.3"]]
  :profiles {:dev {:plugins [[codox "0.8.11"]]}})
