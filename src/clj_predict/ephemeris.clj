(ns clj-predict.ephemeris
  "Clojure wrapper to the *predict4java*
   [SGP4](http://en.wikipedia.org/wiki/Simplified_perturbations_models)
   satellite ephemeris propagation library."
  (:require [clj-predict.coordinates :as coord]
            [clj-predict.time :as time])
  (:import [uk.me.g4dpz.satellite SatelliteFactory TLE]))

(def iss
  ["ISS (ZARYA)"
   "1 25544U 98067A   15129.86961041  .00015753  00000-0  23097-3 0  9998"
   "2 25544  51.6464 275.3867 0006524 289.1638 208.5861 15.55704207942078"])

;;;; NORAD TLE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn str->tle
  "Convert a *Two Line Element (TLE)* set, into a usable `predict4java` TLE
   object. Takes a single vector containing text strings of the form
   `[name first-line second-line]` where:

   > `name` - The name of the spacecraft  
   > `first-line` - Line 1 of the orbital elements  
   > `second-line` - Line 2 of the orbital elements

   Returns a `predict4java` TLE object for use with the SGP4 propagator."
  [[name first-line second-line :as tle]]
  (TLE. (into-array String tle)))

(defn valid-tle?
  "Determine if a *Two Line Element Set (TLE)* is valid based on the checksum
   value for each line. Takes a single vector containing text strings of the
   form `[name first-line second-line]` where:

   > `name` - The name of the spacecraft  
   > `first-line` - Line 1 of the orbital elements  
   > `second-line` - Line 2 of the orbital elements

   Returns `true` if the TLE vector appears properly formatted and passes a
   checksum."
  [[name first-line second-line :as tle]]
  (let [char->int #(Character/getNumericValue %)
        digits (set (map char (range 48 58)))
        replace-dash #(clojure.string/replace % "-" "1")
        valid? #(= (mod (reduce + (butlast %)) 10) (last %))
        tle-clean (->> (map replace-dash (rest tle))
                    (map #(filter digits (apply vector %)))
                    (map #(map char->int (apply vector %))))]
    (and (not (clojure.string/blank? (.trim name)))
         (not (clojure.string/blank? (.trim first-line)))
         (not (clojure.string/blank? (.trim second-line)))
         (every? true? (map valid? tle-clean)))))

;;;; SGP4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn propagate
  [tle date]
  (let [sat-tle (str->tle tle)
        date-delta (-> (.getTime date) (+ 1000) (java.util.Date.))
        factory-1 (doto (SatelliteFactory/createSatellite sat-tle)
                    (.calculateSatelliteVectors date))
        factory-2 (doto (SatelliteFactory/createSatellite sat-tle)
                    (.calculateSatelliteVectors date-delta))
        tmp-1 (.calculateSatelliteGroundTrack factory-1)
        tmp-2 (.calculateSatelliteGroundTrack factory-2)
        loc-1 (coord/wrap-geodetic
                {:phi (.getLatitude tmp-1) :lam (.getLongitude tmp-1)
                 :h (* 1000 (.getAltitude tmp-1)) :t date})
        loc-2 (coord/wrap-geodetic 
                {:phi (.getLatitude tmp-2) :lam (.getLongitude tmp-2)
                 :h (* 1000 (.getAltitude tmp-2)) :t date-delta})
        eci-1 (coord/coordinate loc-1 :eci)
        eci-2 (coord/coordinate loc-2 :eci)
        vel-delta {:i (- (:i eci-2) (:i eci-1)) :j (- (:j eci-2) (:j eci-1))
                   :k (- (:k eci-2) (:k eci-1)) :t date}]
    {:r eci-1 :v vel-delta}))
