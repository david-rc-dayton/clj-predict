(ns clj-predict.coordinates
  "Functions for coordinate system transforms."
  (:require [clj-predict.properties :as props]
            [clj-predict.time :as time]))

;;;; Default Values ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare coordinate)
(declare coordinate-type)

(def coordinate-default (atom :geodetic))

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

(defn geo-radius
  ([coord]
    (geo-radius coord (props/celestial-body)))
  ([coord body]
    (let [phi (:phi (coordinate coord :geodetic-rad))
          a (:semi-major-axis (props/celestial-body body))
          b (:semi-minor-axis (props/celestial-body body))]
      (-> (/ (+ (Math/pow (* a a (Math/cos phi)) 2)
                (Math/pow (* b b (Math/sin phi)) 2))
             (+ (Math/pow (* a (Math/cos phi)) 2)
                (Math/pow (* b (Math/sin phi)) 2)))
        (Math/sqrt)))))

(defn geodetic->geodetic-rad
  [{:keys [lat lon alt t] :as coord}]
  (let [phi (deg->rad lat)
        lam (deg->rad lon)]
    {:phi phi :lam lam :h alt :t t}))

(defn geodetic-rad->geodetic
  [{:keys [phi lam h t] :as coord}]
  (let [lat (rad->deg phi)
        lon (rad->deg lam)]
    {:lat lat :lon lon :alt h :t t}))

(defn ecef->geodetic-rad
  [{:keys [x y z t] :as coord}]
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
  [{:keys [phi lam h t] :as coord}]
  (let [wgs84 (props/celestial-body :earth)
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
    (coordinate coord @coordinate-default))
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
    (let [c-fn #(- (* (nth x %1) (nth y %2)) (* (nth x %3) (nth y %4)))
          c1 [1 2 0]
          c2 [2 0 1]]
      (map c-fn c1 c2 c2 c1))))

(defn magnitude
  [v]
  (let [mag (Math/sqrt (reduce + (map * q)))]
    (map #(/ % mag) q)))

;;;; Quaternions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn quaternion-product
  [p q]
  (when (= (count p) (count q) 4)
    (let [dp (dot (rest p) (rest q))
          cp (cross (rest p) (rest q))
          poq (map #(* (first p) %) (rest q))
          qop (map #(* (first q) %) (rest p))
          sum (map #(+ %1 %2 %3) poq qop cp)
          st (- (* (first p) (first q)) dp)]
      (conj sum st))))

(defn quaternion-conjugate
  [q]
  (when (= (count q) 4)
    (let [st (first q)
          en (map #(* -1 %) (rest q))]
      (conj en st))))
