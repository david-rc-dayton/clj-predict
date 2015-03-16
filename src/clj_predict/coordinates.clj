(ns clj-predict.coordinates)

(def ^:dynamic *adist-method* :cosine)

(def wgs84 
  "Parameters in the 1984 World Geodetic System (WGS84) defining the
   measurements of the Earth's reference ellipsoid. Available 
   parameters are:
     :semi-major-axis  Earth's equatorial radius (in kilometers)
     :semi-minor-axis  Earth's polar radius (in kilometers)
     :ecc-squared      squared eccentricity of the reference ellipsoid"
  (let [a 6378137
        f (/ 1 298.257223563)]
    {:semi-major-axis a
     :semi-minor-axis (* a (- 1 f))
     :ecc-squared (- (* 2 f) (* f f))}))

(defn deg->rad 
  "Converts argument `deg` from degrees to radians."
  [deg]
  (* deg (/ Math/PI 180)))

(defn rad->deg
  "Converts argument `rad` from radians to degrees."
  [rad]
  (* rad (/ 180 Math/PI)))

(defn geo-radius
  "Calculate Earth's geocentric radius (in meters) for a map containing the key
   {:latitude} in degrees."
  [{:keys [latitude]}]
  (let [lat (deg->rad latitude)
        a (:semi-major-axis wgs84)
        b (:semi-minor-axis wgs84)]
    (-> (/ (+ (* (Math/pow (* a a (Math/cos lat)) 2))
              (* (Math/pow (* b b (Math/sin lat)) 2)))
           (+ (Math/pow (* a (Math/cos lat)) 2)
              (Math/pow (* b (Math/sin lat)) 2)))
      (Math/sqrt))))

(defn adist-haversine
  "Calculate angular distance (in degrees) between two points, using maps
   containing the keys {:latitude :longitude} in degrees as arguments. This
   function uses the Haversine Formula (the slowest, most accurate method) for
   angular distance computation."
  [{:keys [latitude longitude] :as start-point}
   {:keys [latitude longitude] :as end-point}]
  (let [p1 (deg->rad (:latitude start-point))
        p2 (deg->rad (:latitude end-point))
        delta-p (deg->rad (- (:latitude end-point)
                             (:latitude start-point)))
        delta-l (deg->rad (- (:longitude end-point)
                             (:longitude start-point)))
        a (+ (Math/pow (Math/sin (/ delta-p 2)) 2)
             (* (Math/cos p1) (Math/cos p2)
                (Math/pow (Math/sin (/ delta-l 2)) 2)))]
    (rad->deg (* 2 (Math/atan2 (Math/sqrt a) (Math/sqrt (- 1 a)))))))

(defn adist-cosine
  "Calculate angular distance (in degrees) between two points, using maps
   containing the keys {:latitude :longitude} in degrees as arguments. This
   function uses the Spherical Law of Cosines (faster, but less accurate than 
   the Haversine Formula) for angular distance computation."
  [{:keys [latitude longitude] :as start-point}
   {:keys [latitude longitude] :as end-point}]
  (let [p1 (deg->rad (:latitude start-point))
        p2 (deg->rad (:latitude end-point))
        delta-l (deg->rad (- (:longitude end-point) 
                             (:longitude start-point)))]
    (rad->deg (Math/acos (* (+ (* (Math/sin p1) (Math/sin p2))
                               (* (Math/cos p1) (Math/cos p2) 
                                  (Math/cos delta-l))))))))

(defn adist-equirect
  "Calculate angular distance (in degrees) between two points, using maps
   containing the keys {:latitude :longitude} in degrees as arguments. This
   function uses the Equirectangular Approximation  (fastest, but lease accurate
   method of calcualtion) for angular distance computation."
  [{:keys [latitude longitude] :as start-point}
   {:keys [latitude longitude] :as end-point}]
  (let [delta-lambda (- (:longitude end-point) (:longitude start-point))
        phi-m (/ (+ (:latitude end-point) (:latitude start-point)) 2)
        x (* delta-lambda (Math/cos (deg->rad phi-m)))
        y (- (:latitude end-point) (:latitude start-point))]
    (Math/sqrt (+ (* x x) (* y y)))))

(defn adist
  "Calculate angular distance (in degrees) between two points, using maps
   containing the keys {:latitude :longitude} in degrees as arguments. The
   algorithm used can be selected by entering one of the following as the first
   argument [:haversine :cosine :equirect]. Differences between each algorithm
   can be found in the adist-[haversine|cosine|equirect] documentation. Default
   method bound in the *adist-method* var."
  ([{:keys [latitude longitude] :as start-point}
    {:keys [latitude longitude] :as end-point}]
    (adist *adist-method* start-point end-point))
  ([^clojure.lang.Keyword algorithm
    {:keys [latitude longitude] :as start-point}
    {:keys [latitude longitude] :as end-point}]
    (let [options {:haversine adist-haversine
                   :cosine    adist-cosine
                   :equirect  adist-equirect}
          method (get options algorithm)]
      (method start-point end-point))))

(defn adist-horizon
  "Calculate the angular distance to horizon from an observer on a sphere with
   a radius equivalent to that of a point on (or near) the Earth's surface,
   denoted location maps containing the keys {:latitude :altitude} in degrees
   and meters respectively. This function returns the angular distance to the
   horizon from nadir in degrees."
  [{:keys [latitude altitude] :as observer}
   {:keys [latitude altitude] :as point}]
  (let [r (+ (geo-radius point) (:altitude point))
        h (+ (geo-radius observer) (:altitude observer))]
    (rad->deg (Math/acos (/ r h)))))

(defn adiam-sphere
  "Calculate the angular diameter for an sphere, given the [distance] from the
   observer to the sphere's center, and the sphere's [diameter]. Both [distance]
   and [diameter] must be in the same units, and the output is in degrees."
  [distance diameter]
  (rad->deg (* 2 (Math/asin (/ diameter (* 2 distance))))))

(defn adiam-disc
  "Calculate the angular diameter for a disc, given the [distance] from the
   observer to the disc, and the disc's [diameter]. Both [distance]
   and [diameter] must be in the same units, and the output is in degrees."
  [distance diameter]
  (rad->deg (* 2 (Math/atan (/ diameter (* 2 distance))))))

(defn earth-visible?
  "Return [true] if a point on the Earth's surface is visible by a satellite
   observer. The method of calculation can be set by passing the algorithm
   type keyword as the first argument. The default algorithm can be bound in
   the *adist-method* var."
  ([{:keys [latitude longitude altitude] :as observer}
    {:keys [latitude longitude altitude] :as point}]
    (earth-visible? *adist-method* observer point))
  ([^clojure.lang.Keyword method
    {:keys [latitude longitude altitude] :as observer}
    {:keys [latitude longitude altitude] :as point}]
    (let [limit (adist-horizon observer point)]
      (<= (adist method observer point) limit))))

(defn destination-point
  "Calculate the destination from a point on the Earth's surface given a
   starting bearing and angular distance in degrees. Outputs a map containing
   the keys {:latitude :longitude} in degrees."
  [{:keys [latitude longitude] :as earth-point} bearing adist]
  (let [phi-1 (deg->rad latitude)
        lam-1 (deg->rad longitude)
        theta (deg->rad bearing)
        sigma (deg->rad adist)
        phi-2 (Math/asin (+ (* (Math/sin phi-1) (Math/cos sigma))
                            (* (Math/cos phi-1) (Math/sin sigma)
                               (Math/cos theta))))
        lam-2 (+ lam-1 (Math/atan2 (* (Math/sin theta) (Math/sin sigma)
                                      (Math/cos phi-1))
                                   (- (Math/cos sigma)
                                      (* (Math/sin phi-1) (Math/sin phi-2)))))]
    {:latitude (rad->deg phi-2) :longitude (rad->deg lam-2) :altitude 0}))

(defn surface-outline
  "Build a list of points {:latitude :longitude :altitude} outlining a
   satellite's field-of-view. Takes an input satellite location map
   {:latitude :longitude :altitude} and the angular separation [degree-sep]
   between each point radially from the center in degrees."
  [{:keys [latitude longitude altitude] :as satellite} degree-sep]
  (let [bearing-list (range 0 360 degree-sep)
        limit (adist-horizon satellite {:latitude 0 :altitude 0})
        outline-fn #(destination-point satellite % limit)]
    (map outline-fn bearing-list)))

(defn geodetic->ecf
  "Convert a map of Geodetic coordinates containing the keys
   {:latitude :longitude :altitude} in degrees and meters to an Earth Centered
   Fixed coordinate map {:xf :yf :zf} in meters."
  [{:keys [latitude longitude altitude]}]
  (let [phi (deg->rad latitude)
        lambda (deg->rad longitude)
        h altitude
        re (:semi-major-axis wgs84)
        rp (:semi-minor-axis wgs84)
        e-squared (:ecc-squared wgs84)
        sin-phi (Math/sin phi)
        n (/ re (Math/sqrt (- 1 (* e-squared (Math/pow sin-phi 2)))))]
    {:xf (* (+ n h) (Math/cos phi) (Math/cos lambda))
     :yf (* (+ n h) (Math/cos phi) (Math/sin lambda))
     :zf (* (+ (* n (- 1 e-squared)) h) sin-phi)}))

(defn ecf->geodetic
  "Convert a map of Earth Centered Fixed coordinates containing the keys
   {:xf :yf :zf} in meters, to a Geodetic coordinate map 
   {:latitude :longitude :altitude} in degrees and meters, respectively."
  [{:keys [xf yf zf]}]
  (let [lambda (mod (Math/atan2 yf xf) (deg->rad 360))
        p (Math/sqrt (+ (* xf xf) (* yf yf)))
        rp (Math/sqrt (+ (* xf xf) (* yf yf) (* zf zf)))
        l-sign (if (< zf 0) -1 1)
        a (:semi-major-axis wgs84)
        b (:semi-minor-axis wgs84)
        e-squared (:ecc-squared wgs84)
        epsilon 1e-10]
    (if (< p epsilon)
      {:phi (* l-sign 90) :lambda (rad->deg lambda) :h (- rp b)}
      (loop [zi (* (- e-squared) zf)]
        (let [zd (- zf zi)
              n-plus-h (Math/sqrt (+ (* xf xf) (* yf yf) (* zd zd)))
              sin-phi (/ zd n-plus-h)
              n (/ a (Math/sqrt (- 1 (* e-squared sin-phi sin-phi))))
              zi-next (* (- n) e-squared sin-phi)
              delta-zi (Math/abs (- zi zi-next))]
          (if (< delta-zi epsilon)
            (let [nlam (rad->deg lambda)]
              {:latitude (rad->deg (Math/asin (/ zd n-plus-h)))
               :longitude (cond
                            (> nlam 180)  (- nlam 360)
                            (< nlam -180) (+ nlam 360)
                            :else nlam)
               :altitude (- (Math/sqrt (+ (* xf xf) (* yf yf) (* zd zd))) n)})
            (recur zi-next)))))))

(defn azimuth
  "Calculate the azimuth between an earth-station and a satellite, using maps
   containing the keys {:latitude :longitude} in degrees and meters,
   respectively. Azimuth is returned in degrees from true north."
  [{:keys [latitude longitude] :as earth-station} 
   {:keys [latitude longitude] :as satellite}]
  (let [Le (deg->rad (:latitude earth-station))
        Ls (deg->rad (:latitude satellite))
        ls-le (deg->rad (- (:longitude satellite)
                           (:longitude earth-station)))
        y (* (Math/sin ls-le) (Math/cos Ls))
        x (- (* (Math/cos Le) (Math/sin Ls))
             (* (Math/sin Le) (Math/cos Ls) (Math/cos ls-le)))]
    (mod (+ 360 (rad->deg (Math/atan2 y x))) 360)))

(defn elevation
  "Calculate elevation between an earth-station and satellite using maps
   containing the keys {:latitude :longitude :altitude} in degrees and meters,
   as arguments. Elevation is returned in degrees above the horizon."
  [{:keys [latitude longitude altitude] :as earth-station} 
   {:keys [latitude longitude altitude] :as satellite}]
  (let [A (deg->rad (:latitude earth-station))
        B (deg->rad (:latitude satellite))
        Lt (- (:longitude earth-station) (:longitude satellite))
        L (deg->rad (cond 
                      (> Lt 180)  (- Lt 360)
                      (< Lt -180) (+ Lt 360)
                      :else Lt))
        D (rad->deg (Math/acos (+ (* (Math/sin A) (Math/sin B))
                                  (* (Math/cos A) (Math/cos B) (Math/cos L)))))
        K (/ (+ (geo-radius satellite) (:altitude satellite))
             (+ (geo-radius earth-station) (:altitude earth-station)))
        D-prime (deg->rad (- 90 D))]
    (rad->deg (Math/atan (- (Math/tan D-prime)
                            (/ 1 (* K (Math/cos D-prime))))))))

(defn distance
  "Calculate range between an earth station and a satellite using
   maps containing the keys {:latitude :longitude :altitude} in degrees and
   meters, as arguments. Distance is returned in meters."
  [{:keys [latitude longitude altitude] :as earth-station}
   {:keys [latitude longitude altitude] :as satellite}]
  (let [es-ecf (geodetic->ecf earth-station)
        sat-ecf (geodetic->ecf satellite)
        x-delta (Math/pow (- (:xf es-ecf) (:xf sat-ecf)) 2)
        y-delta (Math/pow (- (:yf es-ecf) (:yf sat-ecf)) 2)
        z-delta (Math/pow (- (:zf es-ecf) (:zf sat-ecf)) 2)]
    (Math/sqrt (+ x-delta y-delta z-delta))))

(defn look-angle
  "Calculate the look angles between earth station and satellite locations,
   using maps containing the keys {:latitude :longitude :altitude} in degrees
   and meters, as arguments. Outputs a map containing:
     :azimuth    (in degrees)
     :elevation  (in degrees)
     :range      (slant-range in meters)
     :visible?   (true if satellite is in view of earth-station)"
  [{:keys [latitude longitude altitude] :as earth-station} 
   {:keys [latitude longitude altitude] :as satellite}]
  (let [az (azimuth earth-station satellite)
        el (elevation earth-station satellite)
        rng (distance earth-station satellite)
        vis? (pos? el)]
    {:azimuth az :elevation el :range rng :visible? vis?}))
