(ns clj-predict.coordinates
  "Functions for working with coordinates and their transforms."
  (:require [clj-predict.properties :as props]
            [clj-predict.time :as time]))

;;;; Default Values ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare coordinate)
(declare coordinate-type)
(declare coordinate-references)

(def coordinate-default
  "Default coordinate frame to return from clj-predict functions. Change
   globally using the coordinate-default! function; defaults to :geodetic.

   See coordinate-references for more options."
  (atom :geodetic))

(defn coordinate-frame
  "Return the coordinate-frame keyword defined in coordinate-default."
  []
  @coordinate-default)

(defn coordinate-default!
  "Change the global default coordinate reference frame stored in
   coordinate-default. For example, to set the default output to Earth Centered
   Intertial (ECI) coordinates:

     (coordinate-default! :eci)

  The keyword argument must exist in the coordinate-references atom."
  [k]
  (assert (k (set (keys @coordinate-references))))
  (reset! coordinate-default k))

;;;; Coordinate Transforms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn deg->rad 
  "Convert from degrees to radians. For example:

     (deg->rad 180) ;=> 3.141592653589793"
  [deg]
  (* deg (/ Math/PI 180)))

(defn rad->deg
  "Convert from radians to degrees. For example:

     (rad->deg Math/PI) ;=> 180.0"
  [rad]
  (* rad (/ 180 Math/PI)))

(defn geo-radius
  "Calculate the radius of a celestial body, in kilometers, at a given
   coordinate. Body defualts to :earth. For example,

   To calculate the Earth's radius at [45N, 90W]:

     (geo-radius {:lat 45 :lon -90 :alt 0}) ;=> 6367.489577588282

   To calculate Mars' radius at the Meridian:

     (geo-radius {:phi 0 :lam 0 :h 0} :mars) ;=> 3396.2"
  ([coord]
    (geo-radius coord (props/celestial-body)))
  ([coord body]
    (let [phi (:phi (coordinate coord :geodetic-rad))
          a (:semi-major-axis (props/celestial-map body))
          b (:semi-minor-axis (props/celestial-map body))]
      (-> (/ (+ (Math/pow (* a a (Math/cos phi)) 2)
                (Math/pow (* b b (Math/sin phi)) 2))
             (+ (Math/pow (* a (Math/cos phi)) 2)
                (Math/pow (* b (Math/sin phi)) 2)))
        (Math/sqrt)))))

(defn geodetic->geodetic-rad
  "Convert Geodetic coordinates from degrees to radians. For example, to convert
   [10S, 25E] from degrees to radians:

     (geodetic->geodetic-rad {:lat -10 :lon 25 :alt 0})
       ;=> {:phi -0.17453292519943295, :lam 0.4363323129985824, :h 0}"
  [{:keys [lat lon alt t] :as coord}]
  (let [phi (deg->rad lat)
        lam (deg->rad lon)]
    {:phi phi :lam lam :h alt :t t}))

(defn geodetic-rad->geodetic
  "Convert Geodetic coordinates from radians to degrees. For example, to convert
   [-1φ, 3λ] from radians to degrees:

     (geodetic-rad->geodetic {:phi -1 :lam 3 :h 0})
       ;=> {:lat -57.29577951308232, :lon 171.88733853924697, :alt 0}"
  [{:keys [phi lam h t] :as coord}]
  (let [lat (rad->deg phi)
        lon (rad->deg lam)]
    {:lat lat :lon lon :alt h :t t}))

(defn ecef->geodetic-rad
  "Convert Earth Centered Earth Fixed (ECEF) coordinates, in kilometers, to
   Geodetic coordinates, in radians. For example, to convert
   [X: 3304.536 km, Y: -3081.53 km, Z: 4488.14 km] to Geodetic radians:

     (ecef->geodetic-rad {:x 3304.536 :y -3081.53 :z 4488.14})
       ;=> {:phi 0.78539808918036, :lam -0.75049163133736, :h 1.1198495949074}"
  [{:keys [x y z t] :as coord}]
  (let [epsilon 1e-10
        wgs84 (props/celestial-map :earth)
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
        {:phi pole-phi :lam pole-lambda :h pole-height :t t})
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
              {:phi phi :lam lam :h h :t t})))))))

(defn geodetic-rad->ecef
  "Convert Geodetic coordinates, in radians, to Earth Centered Earth Fixed
   (ECEF) coordinates, in kilometers. For example, to convert [0.5φ, 0.7λ] to
   ECEF coordinates:

     (geodetic-rad->ecef {:phi 0.5 :lam 0.7 :h 0})
       ;=> {:x 4284.380588898315, :y 3608.6839875106157, :z 3039.710964467793}"
  [{:keys [phi lam h t] :as coord}]
  (let [wgs84 (props/celestial-map :earth)
        re (:semi-major-axis wgs84)
        e-squared (:ecc-squared wgs84)
        sin-phi (Math/sin phi)
        cos-phi (Math/cos phi)
        sin-lam (Math/sin lam)
        cos-lam (Math/cos lam)
        n (/ re (Math/sqrt (- 1 (* e-squared (Math/pow sin-phi 2)))))]
    {:x (* (+ n h) cos-phi cos-lam)
     :y (* (+ n h) cos-phi sin-lam)
     :z (* (+ (* n (- 1 e-squared)) h) sin-phi)
     :t t}))

(defn geodetic-rad->eci
  "Convert Geodetic coordinates, in radians, to Earth Centered Inertial (ECI)
   coordinates, in kilometers. For example, to convert [0.2φ, -0.4λ] to ECI
   coordinates at 12:33:15z on JDay 15-212:

     (def date (clj-predict.time/date-parse \"15-212 12:33:15\"))
     (geodetic-rad->eci {:phi 0.2 :lam -0.4 :h 0 :t date})
       ;=> {:i -2576.337260445, :j 5696.29711656, :k 1258.823834482}"
  [{:keys [phi lam h t] :as coord}]
  (let [g (time/gmst t)
        e (geodetic-rad->ecef coord)
        i (+ (* (:x e) (Math/cos g))
             (- (* (:y e) (Math/sin g))))
        j (+ (* (:x e) (Math/sin g))
             (* (:y e) (Math/cos g)))
        k (:z e)]
    {:i i :j j :k k :t t}))

(defn eci->geodetic-rad
  "Convert Earth Centered Inertial (ECI) coordinates, in kilometers, to Geodetic
   coordinates, in radians. For example, to convert
   [I: -1000 km, J: 600 km, K: 850 km] to Geodetic radians at 09:23z:

     (eci->geodetic-rad {:i -1000 :j 600 :k 850}"
  [{:keys [i j k t] :as coord}]
  (let [g (time/gmst t)
        x (+ (* i (Math/cos g))
             (* j (Math/sin g)))
        y (+ (- (* i (Math/sin g)))
             (* j (Math/cos g)))
        z k]
    (ecef->geodetic-rad {:x x :y y :z z :t t})))

(defn wrap-geodetic
  [coord]
  (let [t (coordinate-type coord)
        d (condp = t
            :geodetic     {:k :lon :fn identity}
            :geodetic-rad {:k :lam :fn deg->rad})
        k (:k d)
        f (:fn d)
        v (get coord k)
        pl (cond
             (> v (f 180)) (- v (f 360))
             (< v (f 0))   (+ v (f 360))
             :else v)]
    (merge coord {k pl})))

;;;; Helper Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def coordinate-references
  (atom {; Latitude Longitude Height (radians) *** conversion reference ***
         :geodetic-rad {:input identity
                        :output identity
                        :format #{:phi :lam :h :t}}
         ; Latitude Longitude Height (degrees)
         :geodetic     {:input geodetic->geodetic-rad
                        :output geodetic-rad->geodetic
                        :format #{:lat :lon :alt :t}}
         ; Earth-Centered, Earth-Fixed
         :ecef         {:input ecef->geodetic-rad
                        :output geodetic-rad->ecef
                        :format #{:x :y :z :t}}
         ; Earth-Centered Inertial
         :eci          {:input eci->geodetic-rad
                        :output geodetic-rad->eci
                        :format #{:i :j :k :t}}}))

(defn coordinate-referencess!
  ([name]
    (swap! coordinate-references dissoc name))
  ([name coord-map]
    (swap! coordinate-references merge {name coord-map})))

(defn coordinate-type
  [coord]
  (let [match? (fn [m] (every? (:format (val m)) (keys coord)))]
    (key (or (first (filter match? @coordinate-references))
             (first {:unknown nil})))))

(defn coordinate
  ([coord]
    (coordinate coord (coordinate-frame)))
  ([coord output]
    (let [t-coord (if (:t coord) coord (merge coord {:t (java.util.Date.)}))
          in-coord-type (coordinate-type t-coord)
          input-coord-map (get @coordinate-references in-coord-type)
          output-coord-map (get @coordinate-references output)]
      ; throw exception if either coordinate type is unknown
      (when (nil? input-coord-map)
        (throw (Exception. (str "***coordinate reference frame unknown*** "
                                (keys t-coord)))))
      (when (nil? output-coord-map)
        (throw (Exception. (str "***coordinate reference frame unknown*** "
                                output))))
      ; do nothing if already the correct type
      (if (= in-coord-type output)
        t-coord
        ; otherwise, complete the conversion
        (let [input-fn (:input input-coord-map)
              output-fn (:output output-coord-map)]
          (-> (input-fn t-coord) (output-fn)))))))

;;;; Vector Ops ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dot
  [mx my]
  (->> (interleave mx my) (partition 2 2)
    (map #(apply * %)) (reduce +)))

(defn cross
  [mx my]
  (when (= (count mx) (count my) 3)
    (let [c-fn #(- (* (nth mx %1) (nth my %2))
                   (* (nth mx %3) (nth my %4)))
          c1 [1 2 0]
          c2 [2 0 1]]
      (map c-fn c1 c2 c2 c1))))

(defn magnitude
  [v]
  (Math/sqrt (reduce + (map #(* % %) v))))

(defn normalize
  [v]
  (let [mag (magnitude v)]
    (map #(/ % mag) v)))

;;;; Quaternions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn q-product
  [p q]
  (when (= (count p) (count q) 4)
    (let [dp (dot (rest p) (rest q))
          cp (cross (rest p) (rest q))
          poq (map #(* (first p) %) (rest q))
          qop (map #(* (first q) %) (rest p))
          sum (map #(+ %1 %2 %3) poq qop cp)
          st (- (* (first p) (first q)) dp)]
      (conj sum st))))

(defn q-conjugate
  [q]
  (when (= (count q) 4)
    (let [st (first q)
          en (map #(* -1 %) (rest q))]
      (conj en st))))
