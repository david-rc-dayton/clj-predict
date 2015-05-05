(ns clj-predict.core
  "Contains aliases to common *clj-predict* functions."
  (:require [clj-predict.coordinates :as coord]
            [clj-predict.coverage :as cov]
            [clj-predict.propagation :as prop]
            [clj-predict.solar :as sol]))

;;;; clj-predict.coordinates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn geo-radius
  "Alias for `clj-predict.coordinates/geo-radius`."
  [& rest]
  (apply coord/geo-radius rest))

(defn angular-distance
  "Alias for `clj-predict.coordinates/angular-distance`."
  [& rest]
  (apply coord/angular-distance rest))

(defn distance-to-horizon
  "Alias for `clj-predict.coordinates/distance-to-horizon`."
  [& rest]
  (apply coord/distance-to-horizon rest))

(defn angular-diameter
  "Alias for `clj-predict.coordinates/angular-diameter`."
  [& rest]
  (apply coord/angular-diameter rest))

(defn surface-visible?
  "Alias for `clj-predict.coordinates/surface-visible?`."
  [& rest]
  (apply coord/surface-visible? rest))

(defn look-angle
  "Alias for `clj-predict.coordinates/look-angle`."
  [& rest]
  (apply coord/look-angle rest))

(defn aspect-angle
  "Alias for `clj-predict.coordinates/aspect-angle`."
  [& rest]
  (apply coord/aspect-angle rest))

;;;; clj-predict.coverage ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn print-coverage-matrix
  "Alias for `clj-predict.coverage/print-coverage-matrix`."
  [& rest]
  (apply cov/print-coverage-matrix rest))

(defn merge-ascii-legend!
  "Alias for `clj-predict.coverage/merge-ascii-legend!`."
  [& rest]
  (apply cov/merge-ascii-legend! rest))

(defn coverage-matrix
  "Alias for `clj-predict.coverage/coverage-matrix`."
  [& rest]
  (apply cov/coverage-matrix rest))

;;;; clj-predict.propagation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn valid-tle?
  "Alias for `clj-predict.propagation/valid-tle?`."
  [& rest]
  (apply prop/valid-tle? rest))

(defn propagate
  "Alias for `clj-predict.propagation/propagate`."
  [& rest]
  (apply prop/propagate rest))

;;;; clj-predict.solar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn solar-position
  "Alias for `clj-predict.solar/solar-position`."
  [& rest]
  (apply sol/solar-position rest))
