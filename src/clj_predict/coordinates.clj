(ns clj-predict.coordinates)

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

(defn ^{:private true} deg->rad 
  "Converts argument `deg` from degrees to radians."
  [deg]
  (* deg (/ Math/PI 180)))

(defn ^{:private true} rad->deg
  "Converts argument `rad` from radians to degrees."
  [rad]
  (* rad (/ 180 Math/PI)))

(defn geo-radius
  "Calculate Earth's geocentric radius for a map containing the key `:lat` in
   degrees.

   Returns the distance between the center of the Earth and the surface of the
   `wgs84` reference ellipsoid for the given latitude, in meters."
  [{:keys [lat]}]
  (let [phi (deg->rad lat)
        a (:semi-major-axis wgs84)
        b (:semi-minor-axis wgs84)]
    (-> (/ (+ (* (Math/pow (* a a (Math/cos phi)) 2))
              (* (Math/pow (* b b (Math/sin phi)) 2)))
           (+ (Math/pow (* a (Math/cos phi)) 2)
              (Math/pow (* b (Math/sin phi)) 2)))
      (Math/sqrt))))

(defn adist-haversine
  "Calculate angular distance, in degrees, between two points on the Earth's
   surface using the Haversine Formula (the slowest, most accurate method) for
   angular distance computation. Takes two maps as arguments, with values in
   degrees:

   > `start-point` - Map of the `:lat :lon` for the start point  
   > `end-point` - Map of the `:lat :lon` for the end point

   Returns the angular distance between the two points in degrees."
  [{:keys [lat lon] :as start-point}
   {:keys [lat lon] :as end-point}]
  (let [p1 (deg->rad (:lat start-point))
        p2 (deg->rad (:lat end-point))
        delta-p (deg->rad (- (:lat end-point)
                             (:lat start-point)))
        delta-l (deg->rad (- (:lon end-point)
                             (:lon start-point)))
        a (+ (Math/pow (Math/sin (/ delta-p 2)) 2)
             (* (Math/cos p1) (Math/cos p2)
                (Math/pow (Math/sin (/ delta-l 2)) 2)))]
    (rad->deg (* 2 (Math/atan2 (Math/sqrt a) (Math/sqrt (- 1 a)))))))

(defn adist-cosine
  "Calculate angular distance, in degrees, between two points on the Earth's
   surface using the Spherical Law of Cosines (faster, but less accurate than 
   the Haversine Formula) for angular distance computation. Takes two maps as
   arguments, with values in degrees:

   > `start-point` - Map of the `:lat :lon` for the start point  
   > `end-point` - Map of the `:lat :lon` for the end point

   Returns the angular distance between the two points in degrees."
  [{:keys [lat lon] :as start-point}
   {:keys [lat lon] :as end-point}]
  (let [p1 (deg->rad (:lat start-point))
        p2 (deg->rad (:lat end-point))
        delta-l (deg->rad (- (:lon end-point) 
                             (:lon start-point)))]
    (rad->deg (Math/acos (* (+ (* (Math/sin p1) (Math/sin p2))
                               (* (Math/cos p1) (Math/cos p2) 
                                  (Math/cos delta-l))))))))

(defn adist-equirect
  "Calculate angular distance, in degrees, between two points on the Earth's
   surface using the Equirectangular Approximation (fastest, but least accurate
   method of calculation) for angular distance computation. Takes the arguments:

   > `start-point` - Map of the `:lat :lon` for the start point  
   > `end-point` - Map of the `:lat :lon` for the end point

   Returns the angular distance between the two points in degrees."
  [{:keys [lat lon] :as start-point}
   {:keys [lat lon] :as end-point}]
  (let [delta-lambda (- (:lon end-point) (:lon start-point))
        phi-m (/ (+ (:lat end-point) (:lat start-point)) 2)
        x (* delta-lambda (Math/cos (deg->rad phi-m)))
        y (- (:lat end-point) (:lat start-point))]
    (Math/sqrt (+ (* x x) (* y y)))))

(defn horizon
  "Calculate the angular distance to horizon from an observer above the Earth's
   surface. Takes one argument, a map of the observer's `:lat :alt` in degrees
   and meters.

   Returns the angular distance from the observer's nadir to the horizon, in
   degrees."
  [{:keys [lat alt] :as observer}]
  (let [r (geo-radius observer)]
    (rad->deg (Math/acos (/ r (+ r alt))))))

(defn adiam-sphere
  "Calculate the angular diameter of a sphere, given the `distance` from the
   observer to the sphere's center, and the sphere's `diameter`. Both `distance`
   and `diameter` must be in the same units.

   Returns the angular diameter of the sphere, relative to the observer, in
   degrees."
  [distance diameter]
  (rad->deg (* 2 (Math/asin (/ diameter (* 2 distance))))))

(defn adiam-disc
  "Calculate the angular diameter of a disc, given the `distance` from the
   observer to the disc, and the disc's `diameter`. Both `distance`
   and `diameter` must be in the same units.

   Returns the angular diameter of the disc, relative to the observer, in
   degrees."
  [distance diameter]
  (rad->deg (* 2 (Math/atan (/ diameter (* 2 distance))))))

(defn surface-visible?
  "Calculate the visibility of a point on the Earth's surface from a satellite
   observer. Takes the arguments:

   > `observer` - Map with keys `:lat :lon :alt` for the observer's location  
   > `point` - Map with keys `:lat :lon :alt` for the point's location  
   > `adist-fn` - Angular distance function (see `adist-*` functions)

   Returns `true` if the point on the Earth's surface is visible from the
   observer."
  ([{:keys [lat lon alt] :as observer}
    {:keys [lat lon alt] :as point}
    adist-fn]
    (let [limit (horizon observer)]
      (<= (adist-fn observer point) limit))))

(defn ^{:private true} destination-point
  "Calculate the destination from a point on the Earth's surface when traveled
   in a set direction for a defined distance. Takes three arguments, in degrees:

   > `start-point` - Map of the `:lat :lon` for the start point  
   > `bearing` - Direction of travel in degrees (0 to 360); zero is true north  
   > `adist` - Angular distance of travel, in degrees

   Outputs a map containing the keys `{:lat :lon :alt}` for the
   end point, in degrees."
  [{:keys [lat lon] :as start-point} bearing adist]
  (let [phi-1 (deg->rad lat)
        lam-1 (deg->rad lon)
        theta (deg->rad bearing)
        sigma (deg->rad adist)
        phi-2 (Math/asin (+ (* (Math/sin phi-1) (Math/cos sigma))
                            (* (Math/cos phi-1) (Math/sin sigma)
                               (Math/cos theta))))
        lam-2 (+ lam-1 (Math/atan2 (* (Math/sin theta) (Math/sin sigma)
                                      (Math/cos phi-1))
                                   (- (Math/cos sigma)
                                      (* (Math/sin phi-1) (Math/sin phi-2)))))]
    {:lat (rad->deg phi-2) :lon (rad->deg lam-2) :alt 0}))

(defn horizon-outline
  "Build a list of points outlining a satellite's field-of-view. Takes the
   arguments:

   > `satellite` - Map of keys `:lat :lon :alt` for the satellite location  
   > `degree-sep` - Degrees of radial separation from nadir between points

   Returns a list of points outlining the satellite's field-of-view; each point
   contains the keys `:lat :lon :alt` in degrees and meters respectively."
  [{:keys [lat lon alt] :as satellite} degree-sep]
  (let [bearing-list (range 0 360 degree-sep)
        limit (horizon satellite)
        outline-fn #(destination-point satellite % limit)]
    (map outline-fn bearing-list)))

(defn geodetic->ecf
  "Convert a map of Geodetic coordinates to Earth Centered Fixed (ECF)
   coordinates. Takes a map containing `:lat :lon :alt` keys, with values in
   degrees and meters as its only argument.

   Returns a map containing ECF coordinates, with keys `:xf :yf :zf`, in
   meters."
  [{:keys [lat lon alt]}]
  (let [phi (deg->rad lat)
        lambda (deg->rad lon)
        h alt
        re (:semi-major-axis wgs84)
        rp (:semi-minor-axis wgs84)
        e-squared (:ecc-squared wgs84)
        sin-phi (Math/sin phi)
        n (/ re (Math/sqrt (- 1 (* e-squared (Math/pow sin-phi 2)))))]
    {:xf (* (+ n h) (Math/cos phi) (Math/cos lambda))
     :yf (* (+ n h) (Math/cos phi) (Math/sin lambda))
     :zf (* (+ (* n (- 1 e-squared)) h) sin-phi)}))

(defn ecf->geodetic
  "Convert a map of Earth Centered Fixed (ECF) coordinates to a Geodetic
   coordinates. Takes a map containing the keys `:xf :yf :zf`, with values in
   meters, as its only argument.

   Returns a map containing Geodetic coordinates, with keys `:lat :lon :alt`,
   and values in degrees and meters."
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
              {:lat (rad->deg (Math/asin (/ zd n-plus-h)))
               :lon (cond
                      (> nlam 180)  (- nlam 360)
                      (< nlam -180) (+ nlam 360)
                      :else nlam)
               :alt (- (Math/sqrt (+ (* xf xf) (* yf yf) (* zd zd))) n)})
            (recur zi-next)))))))

(defn ^{:private true} azimuth
  "Calculate the azimuth between an earth-station and a satellite. Takes two
   maps as arguments, in degrees:

   > `earth-station` - Map of the `:lat :lon` for an earth-station  
   > `satellite` - Map of the `:lat :lon` for a satellite

   Returns the antenna azimuth in degrees from true north."
  [{:keys [lat lon] :as earth-station} 
   {:keys [lat lon] :as satellite}]
  (let [Le (deg->rad (:lat earth-station))
        Ls (deg->rad (:lat satellite))
        ls-le (deg->rad (- (:lon satellite)
                           (:lon earth-station)))
        y (* (Math/sin ls-le) (Math/cos Ls))
        x (- (* (Math/cos Le) (Math/sin Ls))
             (* (Math/sin Le) (Math/cos Ls) (Math/cos ls-le)))]
    (mod (+ 360 (rad->deg (Math/atan2 y x))) 360)))

(defn ^{:private true} elevation
  "Calculate the elevation between an earth-station and a satellite. Takes two
   maps as arguments, in degrees and meters:

   > `earth-station` - Map of the `:lat :lon :alt` for an earth-station  
   > `satellite` - Map of the `:lat :lon :alt` for a satellite

   Returns the antenna elevation in degrees above-the-horizon."
  [{:keys [lat lon alt] :as earth-station} 
   {:keys [lat lon alt] :as satellite}]
  (let [A (deg->rad (:lat earth-station))
        B (deg->rad (:lat satellite))
        Lt (- (:lon earth-station) (:lon satellite))
        L (deg->rad (cond 
                      (> Lt 180)  (- Lt 360)
                      (< Lt -180) (+ Lt 360)
                      :else Lt))
        D (rad->deg (Math/acos (+ (* (Math/sin A) (Math/sin B))
                                  (* (Math/cos A) (Math/cos B) (Math/cos L)))))
        K (/ (+ (geo-radius satellite) (:alt satellite))
             (+ (geo-radius earth-station) (:alt earth-station)))
        D-prime (deg->rad (- 90 D))]
    (rad->deg (Math/atan (- (Math/tan D-prime)
                            (/ 1 (* K (Math/cos D-prime))))))))

(defn ^{:private true} distance
  "Calculate the distance between an earth-station and a satellite. Takes two
   maps as arguments, in degrees and meters:

   > `earth-station` - Map of the `:lat :lon :alt` for an earth-station  
   > `satellite` - Map of the `:lat :lon :alt` for a satellite

   Returns the distance between the earth-station and satellite in meters."
  [{:keys [lat lon alt] :as earth-station}
   {:keys [lat lon alt] :as satellite}]
  (let [es-ecf (geodetic->ecf earth-station)
        sat-ecf (geodetic->ecf satellite)
        x-delta (Math/pow (- (:xf es-ecf) (:xf sat-ecf)) 2)
        y-delta (Math/pow (- (:yf es-ecf) (:yf sat-ecf)) 2)
        z-delta (Math/pow (- (:zf es-ecf) (:zf sat-ecf)) 2)]
    (Math/sqrt (+ x-delta y-delta z-delta))))

(defn look-angle
  "Calculate the look-angle between earth station and satellite locations. Takes
   two maps as arguments, with values in degrees and meters:

   > `earth-station` - Map of the `:lat :lon :alt` for an earth-station  
   > `satellite` - Map of the `:lat :lon :alt` for a satellite

   Returns the antenna look-angle as a map containing the keys:

   > `:az` - Antenna azimuth in degrees; zero is true north  
   > `:el` - Antenna elevation in degrees above-the-horizon  
   > `:rng` - Distance between the earth-station and satellite, in meters  
   > `:vis?` - `true` if the satellite is in view of the antenna"
  [{:keys [lat lon alt] :as earth-station} 
   {:keys [lat lon alt] :as satellite}]
  (let [az (azimuth earth-station satellite)
        el (elevation earth-station satellite)
        rng (distance earth-station satellite)
        vis? (pos? el)]
    {:az az :el el :rng rng :vis? vis?}))
