(defproject clj-predict "0.0.1-SNAPSHOT"
  :description "Library for satellite propagation and other space maths."
  :resource-paths ["lib/predict4java-1.1.158.4.jar"
                   "lib/commons-lang-2.6.jar"]
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :javac-options ["-target" "1.6" "-source" "1.6" "-Xlint:-options"]
  :aot [clj-predict.core]
  :main clj-predict.core)
