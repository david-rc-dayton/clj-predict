(ns clj-predict.ephemeris
  "Clojure wrapper to the *predict4java*
   [SGP4](http://en.wikipedia.org/wiki/Simplified_perturbations_models)
   satellite ephemeris propagation library."
  (:require [clj-predict.coordinates :as coord]
            [clj-predict.properties :as props]
            [clj-predict.time :as time])
  (:import [uk.me.g4dpz.satellite SatelliteFactory TLE]))

(def iss-tle
  ["ISS (ZARYA)"
   "1 25544U 98067A   15129.86961041  .00015753  00000-0  23097-3 0  9998"
   "2 25544  51.6464 275.3867 0006524 289.1638 208.5861 15.55704207942078"])

(def iss-state
  {:r [3942494.88 2769715.26 4765218.52]
   :v [-1713.962393 6992.931028 -2639.627761]
   :t (time/date-parse "15-131 12:51:39")})

(def test-state
  {:r [5052458.7 1056271.3 5011636.6]
   :v [3858.9872 4276.3114 -4807.0493]
   :t (time/date-parse "12-003 23:05:28")})

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
  "Propagate satellite ephemeris using a *Two Line Element (TLE)* set. Takes two
   arguments:

   > `tle` - Vector containing tle data (see `valid-tle?`)  
   > `date` - A `java.util.Date` object used as the propagation date

   Returns the satellite's location as a map containing the keys
   `:lat :lon :alt`, in degrees and meters."
  ([[name first-line second-line :as tle] date]
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

;;;; Keplerian Elements ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn eccentricity
  ([state-vector]
    (eccentricity state-vector (props/celestial-body)))
  ([state-vector body]
    (let [b (props/celestial-body body)
          r (:r state-vector)
          v (:v state-vector)
          h (coord/cross r v)
          mu (:mu b)
          s (map #(/ % mu) (coord/cross v h))
          e (map #(/ % (coord/magnitude r)) r)]
      (map #(- %1 %2) s e))))

(defn orbital-energy
  ([state-vector]
    (orbital-energy state-vector (props/celestial-body)))
  ([state-vector body]
    (let [b (props/celestial-body body)
          r (coord/magnitude (:r state-vector))
          v (coord/magnitude (:v state-vector))
          v-sq (* v v)
          mu (:mu b)]
      (- (/ v-sq 2) (/ mu r)))))

(defn semi-major-axis
  ([state-vector]
    (semi-major-axis state-vector (props/celestial-body)))
  ([state-vector body]
    (let [b (props/celestial-body body)
          mu (:mu b)
          ep (orbital-energy state-vector body)]
      (- (/ mu (* 2 ep))))))

(defn inclination
  [state-vector]
  (let [r (:r state-vector)
        v (:v state-vector)
        h (coord/cross r v)]
    (coord/rad->deg (Math/acos (/ (nth h 2) (coord/magnitude h))))))

(defn ascending-node
  [state-vector]
  (let [r (:r state-vector)
        v (:v state-vector)
        k [0 0 1]
        h (coord/cross r v)
        n (coord/cross k h)
        o (Math/acos (/ (nth n 0) (coord/magnitude n)))]
    (coord/rad->deg (if (>= (nth n 1) 0) o (- (* 2 Math/PI) o)))))

(defn argument-periapsis
  ([state-vector]
    (argument-periapsis state-vector (props/celestial-body)))
  ([state-vector body]
    (let [r (:r state-vector)
          v (:v state-vector)
          k [0 0 1]
          h (coord/cross r v)
          n (coord/cross k h)
          e (eccentricity state-vector body)
          w (Math/acos (/ (coord/dot n e)
                          (* (coord/magnitude n) (coord/magnitude e))))]
      (coord/rad->deg (if (< (nth e 2) 0) (- (* 2 Math/PI) w) w)))))

(defn true-anomaly
  ([state-vector]
    (true-anomaly state-vector (props/celestial-body)))
  ([state-vector body]
    (let [e (eccentricity state-vector body)
          r (:r state-vector)
          v (:v state-vector)
          m (coord/dot r v)
          an (Math/acos (/ (coord/dot e r)
                           (* (coord/magnitude e) (coord/magnitude r))))]
      (coord/rad->deg (if (neg? m) (- (* 2 Math/PI) an) an)))))

(defn eccentric-anomaly
  ([state-vector]
    (eccentric-anomaly state-vector (props/celestial-body)))
  ([state-vector body]
    (let [e (coord/magnitude (eccentricity state-vector body))
          n (coord/deg->rad (true-anomaly state-vector body))
          ea (Math/acos (/ (+ e (Math/cos n))
                          (+ 1 (* e (Math/cos n)))))]
      (coord/rad->deg (if (>= n Math/PI) (- (* 2 Math/PI) ea) ea)))))

(defn mean-anomaly
  ([state-vector]
    (mean-anomaly state-vector (props/celestial-body)))
  ([state-vector body]
    (let [e (coord/magnitude (eccentricity state-vector body))
          ea (coord/deg->rad (eccentric-anomaly state-vector body))]
      (coord/rad->deg (- ea (* e (Math/sin ea)))))))

(defn orbital-elements
  ([state-vector]
    (orbital-elements state-vector (props/celestial-body)))
  ([state-vector body]
    {:t (:t state-vector)
     :e (coord/magnitude (eccentricity state-vector body))
     :a (semi-major-axis state-vector body)
     :i (inclination state-vector)
     :o (ascending-node state-vector)
     :w (argument-periapsis state-vector body)
     :m (mean-anomaly state-vector body)}))
