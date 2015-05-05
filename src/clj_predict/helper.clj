(ns clj-predict.helper
  "Helper functions to be used in other *clj-predict* namespaces.")

;;;; Reference Look-Up ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn celestial-body-factory
  [semi-major-axis flattening]
  (let [a semi-major-axis
        f flattening
        b (* a (- 1 f))
        r (-> (+ b (* 2 a)) (/ 3))
        e (- (* 2 f) (* f f))]
    {:semi-major-axis a :semi-minor-axis b :mean-radius r
     :coeff-flat f :ecc-squared e}))

(def celestial-bodies
  (atom {:earth   (celestial-body-factory   6378100 0.0033528)
         :moon    (celestial-body-factory   1738130   0.00125)
         :sun     (celestial-body-factory 696342000  0.000009)
         :mercury (celestial-body-factory   2439700         0)
         :venus   (celestial-body-factory   6051800         0)
         :mars    (celestial-body-factory   3396200   0.00589)
         :jupiter (celestial-body-factory  71492000   0.06487)
         :saturn  (celestial-body-factory  60268000   0.09796)
         :uranus  (celestial-body-factory  25559000    0.0229)
         :neptune (celestial-body-factory  24764000    0.0171)}))

(defn celestial-bodies!
  ([name]
    (swap! celestial-bodies dissoc name))
  ([name semi-major-axis flattening]
    (swap! celestial-bodies merge {name (celestial-body-factory
                                          semi-major-axis flattening)})))

(def celestial-default (atom :earth))

(defn celestial-default!
  [name]
  (reset! celestial-default name))

(defn celestial-body
  ([]
    (celestial-body @celestial-default))
  ([name]
    (get @celestial-bodies name)))

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
        wgs84 (celestial-body :earth)
        re (:semi-major-axis wgs84)
        rp (:semi-minor-axis wgs84)
        e-squared (:ecc-squared wgs84)
        sin-phi (Math/sin phi)
        n (/ re (Math/sqrt (- 1 (* e-squared (Math/pow sin-phi 2)))))]
    {:x (* (+ n h) (Math/cos phi) (Math/cos lambda))
     :y (* (+ n h) (Math/cos phi) (Math/sin lambda))
     :z (* (+ (* n (- 1 e-squared)) h) sin-phi)}))

(defn geodetic-rad->ecef
  [{:keys [phi lam alt]}]
  (let [lat (rad->deg phi)
        lon (rad->deg lam)]
    (geodetic->ecef {:lat lat :lon lon :alt alt})))

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
        wgs84 (celestial-body :earth)
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

(defn ecef->geodetic-rad
  [{:keys [x y z]}]
  (let [geo (ecef->geodetic {:x x :y y :z z})]
    {:phi (deg->rad (:lat geo)) :lam (deg->rad (:lon geo)) :alt (:alt geo)}))

(def coordinate-reference
  (atom {; Earth-Centered, Earth-Fixed (used as conversion reference)
         :ecef    {:input identity
                   :output identity
                   :format #{:x :y :z}}
         ; Geodetic Datum (degrees)
         :geodetic {:input geodetic->ecef
                    :output ecef->geodetic
                    :format #{:lat :lon :alt}}
         ; Geodetic Datum (radians)
         :geodetic-rad {:input geodetic-rad->ecef
                        :output ecef->geodetic-rad
                        :format #{:phi :lam :alt}}
         ; Earth-Centered Inertial
         :eci     {:input nil
                   :output nil
                   :format #{:i :j :k}}}))

(defn coordinate-reference!
  ([name]
    (swap! coordinate-reference dissoc name))
  ([name coord-map]
    (swap! coordinate-reference merge {name coord-map})))

(def coordinate-default (atom :geodetic))

(defn coordinate-default!
  [coord-frame]
  (reset! coordinate-default coord-frame))

(defn coordinate-type
  [coords]
  (let [match? (fn [m] (every? (:format (val m)) (keys coords)))]
    (key (or (first (filter match? @coordinate-reference))
             (first {:unknown nil})))))

(defn coordinate-frame
  ([coords]
    (coordinate-frame coords @coordinate-default))
  ([coords output]
    (let [coord-type (coordinate-type coords)
          input-coord-map (get @coordinate-reference coord-type)
          output-coord-map (get @coordinate-reference output)]
      ; throw exception if either coordinate type is unknown
      (if (or (nil? input-coord-map) (nil? output-coord-map))
        (throw (Exception. "*** coordinate reference frame unknown ***"))
        ; do nothing if already the correct type
        (if (= coord-type output)
          coords
          (let [input-fn (:input input-coord-map)
                output-fn (:output output-coord-map)]
            (-> (input-fn coords) output-fn)))))))
