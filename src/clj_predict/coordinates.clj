(ns clj-predict.coordinates
  "Functions for coordinate system transforms."
  (:require [clj-predict.properties :as props]))

;;;; Default Values ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def coordinate-default (atom :llh))

(defn coordinate-default!
  [k]
  (reset! coordinate-default k))

;;;; Coordinate Transforms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn deg->rad 
  "Converts argument `deg` from degrees to radians."
  [deg]
  (* deg (/ Math/PI 180)))

(defn rad->deg
  "Converts argument `rad` from radians to degrees."
  [rad]
  (* rad (/ 180 Math/PI)))

(defn llh-rad->ecef
  [{:keys [phi lambda height]}]
  (let [wgs84 (props/celestial-body :earth)
        re (:semi-major-axis wgs84)
        e-squared (:ecc-squared wgs84)
        sin-phi (Math/sin phi)
        cos-phi (Math/cos phi)
        sin-lam (Math/sin lambda)
        cos-lam (Math/cos lambda)
        n (/ re (Math/sqrt (- 1 (* e-squared (Math/pow sin-phi 2)))))]
    {:x (* (+ n height) cos-phi cos-lam)
     :y (* (+ n height) cos-phi sin-lam)
     :z (* (+ (* n (- 1 e-squared)) height) sin-phi)}))

(defn llh->ecef
  [{:keys [latitude longitude height]}]
  (let [phi (deg->rad latitude)
        lambda (deg->rad longitude)]
    (llh-rad->ecef {:phi phi :lambda lambda :height height})))

(defn ecef->llh-rad
  [{:keys [x y z]}]
  (let [epsilon 1e-10
        wgs84 (props/celestial-body :earth)
        a (:semi-major-axis wgs84)
        b (:semi-minor-axis wgs84)
        e-sq (:ecc-squared wgs84)
        lam (Math/atan2 y x)
        r (-> (+ (* x x) (* y y) (* z z)) (Math/sqrt))
        p (-> (+ (* x x) (* y y)) (Math/sqrt))
        phi-c (Math/atan2 p z)
        rn-fn (fn [pn]
                (/ a (Math/sqrt (- 1 (* e-sq (Math/sin pn) (Math/sin pn))))))
        hn-fn (fn [pn rn]
                (-> (/ p (Math/cos pn)) (- rn)))
        pn-fn (fn [rn hn]
                ; catch divide by zero
                (if (zero? (+ rn hn))
                  (Math/atan (/ z p))
                  (Math/atan (* (/ z p)
                                (/ 1 (- 1 (* e-sq (/ rn (+ rn hn)))))))))]
    ; handle special case at poles
    (if (< p epsilon)
      (let [pole-phi (if (neg? z) (deg->rad -90.0) (deg->rad 90.0))
            pole-lambda lam
            pole-height (- r b)]
        {:phi pole-phi :lambda pole-lambda :height pole-height})
      ; otherwise, iterate toward a solution
      (loop [p-now phi-c]
        (let [r-next (rn-fn p-now)
              h-next (hn-fn p-now r-next)
              p-next (pn-fn r-next h-next)]
          (if (> (Math/abs (- p-now p-next)) epsilon)
            (recur p-next)
            (let [l (+ z (* e-sq (rn-fn p-next) (Math/sin p-next)))
                  phi p-next
                  h (hn-fn phi (rn-fn phi))]
              {:phi phi :lambda lam :height h})))))))

(defn ecef->llh
  [{:keys [x y z]}]
  (let [geo-rad (ecef->llh-rad {:x x :y y :z z})
        latitude (rad->deg (:phi geo-rad))
        longitude (rad->deg (:lambda geo-rad))
        height (:height geo-rad)]
    {:latitude latitude :longitude longitude :height height}))

;;;; Helper Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def coordinate-references
  (atom {; Earth-Centered, Earth-Fixed (used as conversion reference)
         :ecef    {:input identity
                   :output identity
                   :format #{:x :y :z}}
         ; Latitude Longitude Height (degrees)
         :llh     {:input llh->ecef
                   :output ecef->llh
                   :format #{:latitude :longitude :height}}
         ; Latitude Longitude Height (radians)
         :llh-rad {:input llh-rad->ecef
                   :output ecef->llh-rad
                   :format #{:phi :lambda :height}}
         ; Earth-Centered Inertial
         :eci     {:input nil
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
    (coordinate-frame coords @coordinate-default))
  ([coords output]
    (let [in-coord-type (coordinate-type coords)
          input-coord-map (get @coordinate-references in-coord-type)
          output-coord-map (get @coordinate-references output)]
      ; throw exception if either coordinate type is unknown
      (when (nil? input-coord-map)
        (throw (Exception. (str "***coordinate reference frame unknown*** "
                                (keys coords)))))
      (when (nil? output-coord-map)
        (throw (Exception. (str "***coordinate reference frame unknown*** "
                                output))))
      ; do nothing if already the correct type
      (if (= in-coord-type output)
        coords
        ; otherwise, complete the conversion
        (let [input-fn (:input input-coord-map)
              output-fn (:output output-coord-map)]
          (-> (input-fn coords) (output-fn)))))))
