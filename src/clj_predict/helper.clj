(ns clj-predict.helper
  "Helper functions to be used in other *clj-predict* namespaces.")

;;;; Reference Look-Up ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def wgs84 
  "Parameters in the *1984 World Geodetic System (WGS84)* defining the
   measurements of the Earth's reference ellipsoid. Available 
   keys for the returned `wgs84` map are:

   > `:semi-major-axis` - Earth's equatorial radius, in meters  
   > `:semi-minor-axis` - Earth's polar radius, in meters  
   > `:mean-radius` - Mean radius of the Earth, in meters  
   > `:coeff-flat` - Coefficient of flattening for the Earth's surface  
   > `:ecc-squared` - Squared eccentricity of the reference ellipsoid"
  (let [a 6378137
        f (/ 1 298.257223563)
        b (* a (- 1 f))
        r (-> (+ b (* 2 a)) (/ 3))
        e (- (* 2 f) (* f f))]
    {:semi-major-axis a
     :semi-minor-axis b
     :mean-radius r
     :coeff-flat f
     :ecc-squared e}))

;;;; Coordinate Reference Frames ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn deg->rad 
  "Converts argument `deg` from degrees to radians."
  [deg]
  (* deg (/ Math/PI 180)))

(defn rad->deg
  "Converts argument `rad` from radians to degrees."
  [rad]
  (* rad (/ 180 Math/PI)))

(defn geodetic->ecef
  "Convert a map of Geodetic coordinates to Earth Centered Earth Fixed (ECEF)
   coordinates. Takes a map containing `:lat :lon :alt` keys, with values in
   degrees and meters as its only argument.

   Returns a map containing ECEF coordinates, with keys `:x :y :z`, in meters."
  [{:keys [lat lon alt]}]
  (let [phi (deg->rad lat)
        lambda (deg->rad lon)
        h alt
        re (:semi-major-axis wgs84)
        rp (:semi-minor-axis wgs84)
        e-squared (:ecc-squared wgs84)
        sin-phi (Math/sin phi)
        n (/ re (Math/sqrt (- 1 (* e-squared (Math/pow sin-phi 2)))))]
    {:x (* (+ n h) (Math/cos phi) (Math/cos lambda))
     :y (* (+ n h) (Math/cos phi) (Math/sin lambda))
     :z (* (+ (* n (- 1 e-squared)) h) sin-phi)}))

(defn ecef->geodetic
  "Convert a map of Earth Centered Earth Fixed (ECEF) coordinates to a Geodetic
   coordinates. Takes a map containing the keys `:x :y :z`, with values in
   meters, as its only argument.

   Returns a map containing Geodetic coordinates, with keys `:lat :lon :alt`,
   and values in degrees and meters."
  [{:keys [x y z]}]
  (let [lambda (mod (Math/atan2 y x) (deg->rad 360))
        p (Math/sqrt (+ (* x x) (* y y)))
        rp (Math/sqrt (+ (* x x) (* y y) (* z z)))
        l-sign (if (< z 0) -1 1)
        a (:semi-major-axis wgs84)
        b (:semi-minor-axis wgs84)
        e-squared (:ecc-squared wgs84)
        epsilon 1e-10]
    ; correct undefined result at poles
    (if (< p epsilon)
      {:lat (* l-sign 90) :lon (rad->deg lambda) :alt (- rp b)}
      (loop [zi (* (- e-squared) z)]
        (let [zd (- z zi)
              n-plus-h (Math/sqrt (+ (* x x) (* y y) (* z z)))
              sin-phi (/ zd n-plus-h)
              n (/ a (Math/sqrt (- 1 (* e-squared sin-phi sin-phi))))
              zi-next (* (- n) e-squared sin-phi)
              delta-zi (Math/abs (- zi zi-next))]
          (if (< delta-zi epsilon)
            (let [nlam (rad->deg lambda)]
              {:lat (rad->deg (Math/asin (/ zd n-plus-h)))
               :lon (cond
                      (> nlam 180)  (- nlam 360)
                      (< nlam -180) (+ nlam 360)
                      :else nlam)
               :alt (- (Math/sqrt (+ (* x x) (* y y) (* zd zd))) n)})
            (recur zi-next)))))))

(def coordinate-map
  (atom {; Earth-Centered, Earth-Fixed (used as conversion reference)
         :ecef    {:input identity
                   :output identity
                   :format #{:x :y :z}}
         ; Geodetic Datum
         :geodetic {:input geodetic->ecef
                    :output ecef->geodetic
                    :format #{:lat :lon :alt}}
         ; Earth-Centered Inertial
         :eci     {:input nil
                   :output nil
                   :format #{:i :j :k}}}))

(defn merge-coordinate-map!
  [coord-map]
  (swap! coordinate-map merge coord-map))

(defn coordinate-type
  [coords]
  (let [match? (fn [m] (every? (:format (val m)) (keys coords)))]
    (key (or (first (filter match? @coordinate-map))
             (first {:unknown nil})))))

(defn coordinate-frame
  [coords output]
  (let [coord-type (coordinate-type coords)
        input-coord-map (get @coordinate-map coord-type)
        output-coord-map (get @coordinate-map output)]
    ; throw exception if either coordinate type is unknown
    (if (or (nil? input-coord-map) (nil? output-coord-map))
      (throw (Exception. "*** coordinate reference frame unknown ***"))
      ; do nothing if already the correct type
      (if (= coord-type output)
        coords
        (let [input-fn (:input input-coord-map)
              output-fn (:output output-coord-map)]
          (-> (input-fn coords) output-fn))))))
