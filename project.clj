(defproject clj-predict "0.0.1-SNAPSHOT"
  :description "Library for satellite propagation and other space maths."
  :url "https://github.com/david-rc-dayton/clj-predict"
  :repositories {"local" "file:lib"}
  :license {:name "The MIT License (MIT)"
            :url "http://opensource.org/licenses/MIT"
            :distribution :repo}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [commons-lang "2.6"]
                 [predict4java "1.1.158.4"]])
