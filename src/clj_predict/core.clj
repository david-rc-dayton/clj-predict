(ns clj-predict.core
  "Contains aliases to common *clj-predict* functions.")

;;;; clj-predict.coordinates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn geo-radius
  "Alias for `clj-predict.body/geo-radius`."
  [& rest]
  (apply bod/geo-radius rest))

(defn angular-distance
  "Alias for `clj-predict.body/angular-distance`."
  [& rest]
  (apply bod/angular-distance rest))

(defn distance-to-horizon
  "Alias for `clj-predict.body/distance-to-horizon`."
  [& rest]
  (apply bod/distance-to-horizon rest))

(defn angular-diameter
  "Alias for `clj-predict.body/angular-diameter`."
  [& rest]
  (apply bod/angular-diameter rest))

(defn surface-visible?
  "Alias for `clj-predict.body/surface-visible?`."
  [& rest]
  (apply bod/surface-visible? rest))

(defn look-angle
  "Alias for `clj-predict.body/look-angle`."
  [& rest]
  (apply bod/look-angle rest))

(defn aspect-angle
  "Alias for `clj-predict.body/aspect-angle`."
  [& rest]
  (apply bod/aspect-angle rest))

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
