(ns clj-predict.surface
  "Operations applicable to the Earth and other celestial bodies."
  (:require [clj-predict.coordinates :as coord]
            [clj-predict.properties :as props]))

(defn geo-radius
  ([coord]
    (geo-radius coord (props/celestial-body)))
  ([coord body]
    (let [phi (:phi (coord/coordinate-frame coord :llh-rad))
          a (:semi-major-axis (props/celestial-body body))
          b (:semi-minor-axis (props/celestial-body body))]
      (-> (/ (+ (Math/pow (* a a (Math/cos phi)) 2)
                (Math/pow (* b b (Math/sin phi)) 2))
             (+ (Math/pow (* a (Math/cos phi)) 2)
                (Math/pow (* b (Math/sin phi)) 2)))
        (Math/sqrt)))))

(defn adist-haversine
  [start-point end-point]
  (let [s-p (coord/coordinate-frame start-point :llh-rad)
        e-p (coord/coordinate-frame end-point :llh-rad)
        p1 (:phi s-p)
        p2 (:phi e-p)
        delta-p (- (:phi e-p) (:phi s-p))
        delta-l (- (:lambda e-p) (:lambda s-p))
        a (+ (Math/pow (Math/sin (/ delta-p 2)) 2)
             (* (Math/cos p1) (Math/cos p2)
                (Math/pow (Math/sin (/ delta-l 2)) 2)))]
    (coord/rad->deg (* 2 (Math/atan2 (Math/sqrt a) (Math/sqrt (- 1 a)))))))

(defn adist-cosine
  [start-point end-point]
  (let [s-p (coord/coordinate-frame start-point :llh-rad)
        e-p (coord/coordinate-frame end-point :llh-rad)
        p1 (:phi s-p)
        p2 (:phi e-p)
        delta-l (- (:lambda e-p) (:lambda s-p))]
    (coord/rad->deg
      (Math/acos (+ (* (Math/sin p1) (Math/sin p2))
                    (* (Math/cos p1) (Math/cos p2) (Math/cos delta-l)))))))

(defn adist-equirect
  [start-point end-point]
  (let [s-p (coord/coordinate-frame start-point :llh)
        e-p (coord/coordinate-frame end-point :llh)
        delta-lon (- (:longitude e-p) (:longitude s-p))
        lat-m (/ (+ (:latitude e-p) (:latitude s-p)) 2)
        x (* delta-lon (Math/cos (coord/deg->rad lat-m)))
        y (- (:latitude e-p) (:latitude s-p))]
    (Math/sqrt (+ (* x x) (* y y)))))

(def adist-methods
  "Map associating keywords with a method of angular distance calculation.
   Available methods are:

   > `:haversine` - *Haversine Formula* (slow)  
   > `:cosine` - *Law of Cosines* (fast)  
   > `:equirect` - *Equirectangular Approximation* (fastest)"
  {:haversine adist-haversine
   :cosine    adist-cosine
   :equirect  adist-equirect})

(defn angular-distance
  [method start-point end-point]
  (let [adist-fn (get adist-methods method)]
    (adist-fn start-point end-point)))

(defn distance-to-horizon
  ([observer]
    (distance-to-horizon observer (props/celestial-body)))
  ([observer body]
    (let [o-p (coord/coordinate-frame observer :llh)
          r (geo-radius observer body)]
      (coord/rad->deg (Math/acos (/ r (+ r (:height o-p))))))))

(defn surface-visible?
  ([method observer ground]
    (surface-visible? method observer ground (props/celestial-body)))
  ([method observer ground body]
    (let [adist-fn (get adist-methods method)
          limit (distance-to-horizon observer body)]
      (<= (adist-fn observer ground) limit))))

(defn adiam-sphere
  "Calculate the angular diameter of a sphere, given the `distance` from the
   observer to the sphere's center, and the sphere's `diameter`. Both `distance`
   and `diameter` must be in the same units.

   Returns the angular diameter of the sphere, relative to the observer, in
   degrees."
  [distance diameter]
  (coord/rad->deg (* 2 (Math/asin (/ diameter (* 2 distance))))))

(defn adiam-disc
  "Calculate the angular diameter of a disc, given the `distance` from the
   observer to the disc, and the disc's `diameter`. Both `distance`
   and `diameter` must be in the same units.

   Returns the angular diameter of the disc, relative to the observer, in
   degrees."
  [distance diameter]
  (coord/rad->deg (* 2 (Math/atan (/ diameter (* 2 distance))))))

(def adiam-methods
  "Map associating keywords with a method of angular diameter calculation.
   Available methods are:

   > `:sphere` - round objects  
   > `:disc` - flat objects"
  {:sphere adiam-sphere
   :disc   adiam-disc})

(defn angular-diameter
  "Calculate the angular diameter of an object as viewed from an observer. Takes
   three arguments:

   > `shape` - Keyword of the object's shape (see `adiam-methods`)  
   > `distance` - Distance from the observer to the center of the object  
   > `diameter` - Actual diameter of the object being viewed

   `distance` and `diameter` must have the same units.

   Returns the angular diameter of the object, relative to the observer, in
   degrees."
  [shape distance diameter]
  (let [adiam-fn (get adiam-methods shape)]
    (adiam-fn distance diameter)))

(defn azimuth
  [earth-station satellite]
  (let [es (coord/coordinate-frame earth-station :geodetic-rad)
        sat (coord/coordinate-frame satellite :geodetic-rad)
        Le (:phi es)
        Ls (:phi sat)
        ls-le (- (:lam sat) (:lam es))
        y (* (Math/sin ls-le) (Math/cos Ls))
        x (- (* (Math/cos Le) (Math/sin Ls))
             (* (Math/sin Le) (Math/cos Ls) (Math/cos ls-le)))]
    (mod (+ 360 (coord/rad->deg (Math/atan2 y x))) 360)))

(defn elevation
  [earth-station satellite]
  (let [es (coord/coordinate-frame earth-station :geodetic)
        sat (coord/coordinate-frame satellite :geodetic)
        A (coord/deg->rad (:lat es))
        B (coord/deg->rad (:lat satellite))
        Lt (- (:lon es) (:lon sat))
        L (coord/deg->rad (cond 
                            (> Lt 180)  (- Lt 360)
                            (< Lt -180) (+ Lt 360)
                            :else Lt))
        D (coord/rad->deg
            (Math/acos (+ (* (Math/sin A) (Math/sin B))
                          (* (Math/cos A) (Math/cos B) (Math/cos L)))))
        K (/ (+ (geo-radius sat) (:alt sat))
             (+ (geo-radius es) (:alt es)))
        D-prime (coord/deg->rad (- 90 D))]
    (coord/rad->deg (Math/atan (- (Math/tan D-prime)
                                  (/ 1 (* K (Math/cos D-prime))))))))

(defn distance
  [earth-station satellite]
  (let [es (coord/coordinate-frame earth-station :ecef)
        sat (coord/coordinate-frame satellite :ecef)
        x-delta (Math/pow (- (:x es) (:x sat)) 2)
        y-delta (Math/pow (- (:y es) (:y sat)) 2)
        z-delta (Math/pow (- (:z es) (:z sat)) 2)]
    (Math/sqrt (+ x-delta y-delta z-delta))))

(defn look-angle
  [earth-station satellite]
  (let [az (azimuth earth-station satellite)
        el (elevation earth-station satellite)
        rng (distance earth-station satellite)
        vis? (pos? el)]
    {:az az :el el :rng rng :vis? vis?}))

(defn aspect-angle
  [origin point-one point-two]
  (let [a (coord/coordinate-frame origin :ecef)
        b (coord/coordinate-frame point-one :ecef)
        c (coord/coordinate-frame point-two :ecef)
        mag-fn (fn [{:keys [xf yf zf]}]
                 (Math/sqrt (+ (* xf xf) (* yf yf) (* zf zf))))
        a-b {:xf (- (:xf a) (:xf b))
             :yf (- (:yf a) (:yf b))
             :zf (- (:zf a) (:zf b))}
        a-c {:xf (- (:xf a) (:xf c))
             :yf (- (:yf a) (:yf c))
             :zf (- (:zf a) (:zf c))}
        n (+ (* (:xf a-b) (:xf a-c))
             (* (:yf a-b) (:yf a-c))
             (* (:zf a-b) (:zf a-c)))
        d (reduce * (map mag-fn [a-b a-c]))]
    (coord/rad->deg (Math/acos (/ n d)))))