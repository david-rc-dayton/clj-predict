(ns clj-predict.coordinates
  "Functions for working with coordinates and their transforms."
  (:require [clj-predict.properties :as props]
            [clj-predict.time :as time]))

(defn coord-state
  [args]
  (merge {:body :earth
          :time (time/now)}
         (apply hash-map args)))

;; Unit Conversion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn mag
  [v]
  (Math/sqrt (reduce + (map #(* % %) v))))

(defn norm
  [v]
  (let [mag (mag v)]
    (map #(/ % mag) v)))

(defn hyp [a b]
  (Math/sqrt (+ (Math/pow (mag a) 2) (Math/pow (mag b) 2))))

(defn angle [a b]
  (let [n (dot a b)
        m (* (mag a) (mag b))
        theta (rad->deg (Math/acos (/ n m)))]
    [theta (- 360 theta)]))

(defn components-2d [r t]
  (let [theta (deg->rad t)
        x (* (Math/cos theta) r)
        y (* (Math/sin theta) r)]
    [x y]))

(defn components-3d [r t a]
  (let [theta (deg->rad t)
        alpha (deg->rad a)
        r-prime (* r (Math/cos theta))
        x (* (Math/sin theta) r)
        y (* (Math/sin alpha) r-prime)
        z (* (Math/cos alpha) r-prime)]
    [x y z]))

;;;; Quaternion Ops ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;; Coordinate Transforms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn wrap-geo
  [[lat lon alt] & args]
  (let [s (coord-state args)
        wrap-lon (cond
                   (> lon 180) (- lon 360)
                   (< lon 0)   (+ lon 360)
                   :else lon)]
    [lat wrap-lon alt]))

(defn geo-radius
  [latitude & args]
  (let [s (coord-state args)
        phi (deg->rad latitude)
        body (props/body (:body s))
        a (:semi-major-axis body)
        b (:semi-minor-axis body)]
    (-> (/ (+ (Math/pow (* a a (Math/cos phi)) 2)
              (Math/pow (* b b (Math/sin phi)) 2))
           (+ (Math/pow (* a (Math/cos phi)) 2)
              (Math/pow (* b (Math/sin phi)) 2)))
      (Math/sqrt))))

(defn geo->ecf
  [[lat lon alt] & args]
  (let [s (coord-state args)
        body (props/body :earth)
        re (:semi-major-axis body)
        e-squared (:ecc-squared body)
        phi (deg->rad lat)
        lam (deg->rad lon)
        sin-phi (Math/sin phi)
        cos-phi (Math/cos phi)
        sin-lam (Math/sin lam)
        cos-lam (Math/cos lam)
        n (/ re (Math/sqrt (- 1 (* e-squared (Math/pow sin-phi 2)))))]
    [(* (+ n alt) cos-phi cos-lam)
     (* (+ n alt) cos-phi sin-lam)
     (* (+ (* n (- 1 e-squared)) alt) sin-phi)]))

(defn ecf->geo
  [[x y z] & args]
  (let [s (coord-state args)
        epsilon 1e-10
        body (props/body :earth)
        a (:semi-major-axis body)
        b (:semi-minor-axis body)
        e-sq (:ecc-squared body)
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
      (let [pole-phi (rad->deg (if (neg? z) (deg->rad -90.0) (deg->rad 90.0)))
            pole-lambda (rad->deg lam)
            pole-height (- r b)]
        [pole-phi pole-lambda pole-height])
      ; otherwise, iterate toward a solution
      (loop [p-now phi-c]
        (let [r-next (rn-fn p-now)
              h-next (hn-fn p-now r-next)
              p-next (pn-fn r-next h-next)]
          (if (> (Math/abs (- p-now p-next)) epsilon)
            (recur p-next)
            (let [l (+ z (* e-sq (rn-fn p-next) (Math/sin p-next)))
                  phi-out (rad->deg p-next)
                  lam-out (rad->deg lam)
                  h (hn-fn p-next (rn-fn p-next))]
              [phi-out lam-out h])))))))

(defn ecf->eci
  [[x y z] & args]
  (let [s (coord-state args)
        g (time/gmst (:time s))]
    [(+ (* x (Math/cos g)) (- (* y (Math/sin g))))
     (+ (* x (Math/sin g)) (* y (Math/cos g)))
     z]))

(defn eci->ecf
  [[i j k] & args]
  (let [s (coord-state args)
        g (time/gmst (:time s))]
    [(+ (* i (Math/cos g)) (* j (Math/sin g)))
     (+ (- (* i (Math/sin g))) (* j (Math/cos g)))
     k]))
