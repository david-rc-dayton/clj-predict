(ns clj-predict.propagation
  (:import [org.apache.commons.lang StringUtils]
           [uk.me.g4dpz.satellite SatelliteFactory TLE]))

(defn ^{:private true} str->tle
  "Convert a Three Line Element (TLE) set, as a vector containing strings of the
   form [line1 line2 line3] into a 'predict4java' TLE object. [line1] will be
   the name of the spacecraft; [line2] and [line3] will be lines 1 & 2 of the
   orbital elements."
  [[line1 line2 line3 :as tle]]
  (TLE. (into-array String tle)))

(defn valid-tle?
  "Determine if Three Line Element Set is valid based on the checksum value for
   each line. This function takes a vector of strings for [line1 line2 line3].
   Output is [true] if the TLE set format is valid and passes a checksum."
  [[line1 line2 line3 :as tle]]
  (let [char->int #(Character/getNumericValue %)
        digits (set (map char (range 48 58)))
        replace-dash #(clojure.string/replace % "-" "1")
        valid? #(= (mod (reduce + (butlast %)) 10) (last %))
        tle-clean (->> (map replace-dash (rest tle))
                    (map #(filter digits (apply vector %)))
                    (map #(map char->int (apply vector %))))]
    (and (not (clojure.string/blank? (.trim line1)))
         (not (clojure.string/blank? (.trim line2)))
         (not (clojure.string/blank? (.trim line3)))
         (every? true? (map valid? tle-clean)))))

(defn propagate
  "Propagate satellite vectors using a Three Line Element (TLE) set, as a string
   vector of the form [line1 line2 line3] and the date/time as a Java Date
   Object argument. The function's output will be a location map, containing the
   keys {:latitude :longitude :altitude} in degrees and meters respectively. If
   a date is not entered, the current system time is used."
  ([[line1 line2 line3 :as tle]]
    (propagate tle (java.util.Date.)))
  ([[line1 line2 line3 :as tle] ^java.util.Date date]
    (let [tle (str->tle tle)
          factory (doto (SatelliteFactory/createSatellite tle)
                    (.calculateSatelliteVectors date))
          position (.calculateSatelliteGroundTrack factory)
          latitude (Math/toDegrees (.getLatitude position))
          temp-longitude (Math/toDegrees (.getLongitude position))
          longitude (cond
                      (> temp-longitude 180) (- temp-longitude 360)
                      (< temp-longitude 0) (+ temp-longitude 360)
                      :else temp-longitude)
          altitude (*  (.getAltitude position) 1000)]
      {:latitude latitude :longitude longitude :altitude altitude})))
