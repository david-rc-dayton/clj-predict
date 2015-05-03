(ns clj-predict.core
  "Contains aliases to the most common *clj-predict* functions, and a few
   unique functions that combine namespace capabilities."
  (:require [clj-predict.coordinates :as coord]
            [clj-predict.coverage :as cov]
            [clj-predict.propagation :as prop]
            [clj-predict.solar :as sol]))

;;;; Look-Up ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:private true} adist-methods
  {:haversine coord/adist-haversine
   :cosine    coord/adist-cosine
   :equirect  coord/adist-equirect})

(def ^{:private true} adiam-methods
  {:sphere coord/adiam-sphere
   :disc   coord/adiam-disc})

;;;; clj-predict.coordinates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn geo-radius
  "Alias for `clj-predict.coordinates/geo-radius`."
  [& rest]
  (apply coord/geo-radius rest))

(defn horizon
  "Alias for `clj-predict.coordinates/horizon`."
  [& rest]
  (apply coord/horizon rest))

(defn geodetic->ecf
  "Alias for `clj-predict.coordinates/geodetic->ecf`."
  [& rest]
  (apply coord/geodetic->ecf rest))

(defn ecf->geodetic
  "Alias for `clj-predict.coordinates/ecf->geodetic`."
  [& rest]
  (apply coord/ecf->geodetic rest))

(defn look-angle
  "Alias for `clj-predict.coordinates/look-angle`."
  [& rest]
  (apply coord/look-angle rest))

;;;; clj-predict.coverage ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn print-coverage-matrix
  "Alias for `clj-predict.coverage/print-coverage-matrix`."
  [& rest]
  (apply cov/print-coverage-matrix rest))

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

;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn angular-distance
  "Calculate angular distance, in degrees, between two points on the Earth's
   surface using the Haversine Formula (slow/accurate), Spherical Law of
   Cosines (faster/mostly-accurate), or Equirectangular Approximation
   (fastest/least-accurate) for angular distance computation. A keyword, and two
   maps, with values in degrees, as arguments:

   > `method` - Calculation Method (`:haversine`, `:cosine`, or `:equirect`)  
   > `start-point` - Map containing the `:lat :lon` for the start point  
   > `end-point` - Map containing the `:lat :lon` for the end point

   Returns the angular distance between the two points in degrees."
  [method {:keys [lat lon] :as start-point} {:keys [lat lon] :as end-point}]
  (let [adist-fn (get adist-methods method)]
    (adist-fn start-point end-point)))

(defn angular-diameter
  "Calculate the angular diameter of an object as viewed from an observer. Takes
   three arguments:

   > `shape` - Keyword of the object's shape (`:sphere` or `:disc`)  
   > `distance` - Distance from the observer to the center of the object  
   > `diameter` - Actual diameter of the object being viewed

   `distance` and `diameter` must have the same units.

   Returns the angular diameter of the object, relative to the observer, in
   degrees."
  [shape distance diameter]
  (let [adiam-fn (get adiam-methods shape)]
    (adiam-fn distance diameter)))

(defn surface-visible?
  "Calculate the visibility of a point on the Earth's surface from a satellite
   observer. Takes the arguments:

   > `method` - Calculation Method (`:haversine`, `:cosine`, or `:equirect`)  
   > `observer` - Map with keys `:lat :lon :alt` for the observer's location  
   > `point` - Map with keys `:lat :lon` for the point's location

   Returns `true` if the point on the Earth's surface is visible from the
   observer."
  ([method {:keys [lat lon alt] :as observer} {:keys [lat lon] :as point}]
    (let [adist-fn (get adist-methods method)]
      (coord/surface-visible? observer point adist-fn))))

(defn solar-angle
  "Calculate the angle bewteen a satellite, the Earth, and the Sun. Takes two
   arguments:

   > `tle` - Vector containing tle data (see `valid-tle?`)  
   > `date` - A `java.util.Date` object used as the propagation date

   Returns the aspect angle in degrees."
  [[name first-line second-line :as tle] date]
  (let [sat-pos (prop/propagate tle date)
        ear-pos (coord/ecf->geodetic {:xf 0 :yf 0 :zf 0})
        sol-pos (sol/solar-position date)]
    (coord/aspect-angle sat-pos ear-pos sol-pos)))

(defn coverage-matrix
  "Generates a matrix of a single, or multiple satellites' coverage over the
   Earth's surface. Takes three arguments:

   > `method` - keyword from `clj-predict.coverage/cov-methods`  
   > `sat-locations` - satellite location map (individual, or a list of maps)  
   > `dimensions` - a vector containing the width & height of the output matrix

   Returns a matrix representing combined global satellite coverage from
   [-90 90] degrees latitude and [-180 180] degrees longitude, centered at the
   Equator and the Prime Meridian respectively. Elements contain the number of
   satellites in view of the associated region."
  [method sat-location dimensions]
  (let [cov-fn (cond
                 (map? sat-location)  cov/coverage-matrix
                 (coll? sat-location) cov/coverage-combined)]
    (cov-fn method sat-location dimensions)))
