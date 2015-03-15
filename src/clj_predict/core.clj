(ns clj-predict.core
  (:require [clj-predict.coordinates :as coord]
            [clj-predict.propagation :as prop]
            [clj-predict.solar :as sol]))

;;; [clj-predict.coordinates] ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn geo-radius
  [& args]
  "Alias for [clj-predict.coordinates/geo-radius]."
  (apply coord/geo-radius args))

(defn adist-haversine
  [& args]
  "Alias for [clj-predict.coordinates/adist-haversine]."
  (apply coord/adist-haversine args))

(defn adist-cosine
  [& args]
  "Alias for [clj-predict.coordinates/adist-cosine]."
  (apply coord/adist-cosine args))

(defn adist-horizon
  [& args]
  "Alias for [clj-predict.coordinates/adist-horizon]."
  (apply coord/adist-horizon args))

(defn adiam-sphere
  [& args]
  "Alias for [clj-predict.coordinates/adiam-sphere]."
  (apply coord/adiam-sphere args))

(defn adiam-disc
  [& args]
  "Alias for [clj-predict.coordinates/adiam-disc]."
  (apply coord/adiam-disc args))

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
