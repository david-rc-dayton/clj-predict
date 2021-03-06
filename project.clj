(defproject clj-predict "0.0.4-SNAPSHOT"
  :description "Library for satellite propagation and other space maths."
  :url "https://github.com/david-rc-dayton/clj-predict"
  :license {:name "The MIT License (MIT)"
            :url "http://opensource.org/licenses/MIT"
            :distribution :repo}
  :deploy-repositories {"clojars" {:sign-releases false}}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [uk.me.g4dpz/predict4java "1.1.3"]])
