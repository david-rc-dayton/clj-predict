(ns clj-predict.coordinates
  "Functions for coordinate system transforms."
  (:require [clj-predict.properties :as props]))

(def coordinate-default :geodetic)

(defn deg->rad 
  "Converts argument `deg` from degrees to radians."
  [deg]
  (* deg (/ Math/PI 180)))

(defn rad->deg
  "Converts argument `rad` from radians to degrees."
  [rad]
  (* rad (/ 180 Math/PI)))

(defn geodetic->ecef
  [{:keys [lat lon alt]}]
  (let [phi (deg->rad lat)
        lambda (deg->rad lon)
        h alt
        wgs84 (props/celestial-body :earth)
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
  [{:keys [x y z]}]
  (let [lambda (mod (Math/atan2 y x) (deg->rad 360))
        p (Math/sqrt (+ (* x x) (* y y)))
        rp (Math/sqrt (+ (* x x) (* y y) (* z z)))
        l-sign (if (< z 0) -1 1)
        wgs84 (props/celestial-body :earth)
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

(def coordinate-references
  (atom {; Earth-Centered, Earth-Fixed (used as conversion reference)
         :ecef         {:input identity
                        :output identity
                        :format #{:x :y :z}}
         ; Geodetic Datum (degrees)
         :geodetic     {:input geodetic->ecef
                        :output ecef->geodetic
                        :format #{:lat :lon :alt}}
         ; Geodetic Datum (radians)
         :geodetic-rad {:input geodetic-rad->ecef
                        :output ecef->geodetic-rad
                        :format #{:phi :lam :alt}}
         ; Earth-Centered Inertial
         :eci          {:input nil
                        :output nil
                        :format #{:i :j :k}}}))

(defn coordinate-referencess!
  ([name]
    (swap! coordinate-references dissoc name))
  ([name coord-map]
    (swap! coordinate-references merge {name coord-map})))

(defn coordinate-type
  [coords]
  (let [match? (fn [m] (every? (:format (val m)) (keys coords)))]
    (key (or (first (filter match? @coordinate-references))
             (first {:unknown nil})))))

(defn coordinate-frame
  ([coords]
    (coordinate-frame coords coordinate-default))
  ([coords output]
    (let [in-coord-type (coordinate-type coords)
          input-coord-map (get @coordinate-references in-coord-type)
          output-coord-map (get @coordinate-references output)]
      ; throw exception if either coordinate type is unknown
      (if (or (nil? input-coord-map) (nil? output-coord-map))
        (throw (Exception. "*** coordinate reference frame unknown ***"))
        ; do nothing if already the correct type
        (if (= in-coord-type output)
          coords
          ; otherwise, complete the conversion
          (let [input-fn (:input input-coord-map)
                output-fn (:output output-coord-map)]
            (-> (input-fn coords) (output-fn))))))))
