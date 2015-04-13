(defproject clj-predict "0.0.3-SNAPSHOT"
  :description "Library for satellite propagation and other space maths."
  :url "https://github.com/david-rc-dayton/clj-predict"
  :license {:name "The MIT License (MIT)"
            :url "http://opensource.org/licenses/MIT"
            :distribution :repo}
  :deploy-repositories {"clojars" {:sign-releases false}}
  :dependencies [[org.clojure/clojure "1.7.0-beta1"]
                 [uk.me.g4dpz/predict4java "1.1.3"]]
  :global-vars {*unchecked-math* :warn-on-boxed}
  :profiles {:dev {:plugins [[codox "0.8.11"]]
                   :codox {:defaults {:doc/format :markdown}}}})
