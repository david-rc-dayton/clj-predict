(ns clj-predict.propagation
  (:import [uk.me.g4dpz.satellite SatelliteFactory TLE]))

(defn ^{:private true} str->tle
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
  "Propagate satellite ephemeris using a *Two Line Element (TLE)* set. Takes two
   arguments:

   > `tle` - Vector containing tle data (see `valid-tle?`)  
   > `date` - A `java.util.Date` object used as the propagation date

   Returns the satellite's location as a map containing the keys
   `:lat :lon :alt`, in degrees and meters."
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
      {:lat latitude :lon longitude :alt altitude})))
