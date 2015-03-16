(ns clj-predict.core
  (:require [clj-predict.coordinates :as coord]
            [clj-predict.propagation :as prop]
            [clj-predict.solar :as sol]))

;;; [clj-predict.coordinates] ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn adist
  [& args]
  "Alias for [clj-predict.coordinates/adist]."
  (apply coord/adist args))

(defn earth-visible?
  [& args]
  "Alias for [clj-predict.coordinates/earth-visible?]."
  (apply coord/earth-visible? args))

(defn surface-outline
  [& args]
  "Alias for [clj-predict.coordinates/surface-outline]."
  (apply coord/surface-outline args))

(defn geodetic->ecf
  [& args]
  "Alias for [clj-predict.coordinates/geodetic->ecf]."
  (apply coord/geodetic->ecf args))

(defn ecf->geodetic
  [& args]
  "Alias for [clj-predict.coordinates/ecf->geodetic]."
  (apply coord/ecf->geodetic args))

(defn look-angle
  [& args]
  "Alias for [clj-predict.coordinates/look-angle]."
  (apply coord/look-angle args))

;;; [clj-predict.propagation] ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn valid-tle?
  [& args]
  "Alias for [clj-predict.propagation/valid-tle?]."
  (apply prop/valid-tle? args))

(defn propagate
  [& args]
  "Alias for [clj-predict.propagation/propagate]."
  (apply prop/propagate args))

;;; [clj-predict.solar] ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn solar-position
  [& args]
  "Alias for [clj-predict.solar/solar-position]."
  (apply sol/solar-position args))
