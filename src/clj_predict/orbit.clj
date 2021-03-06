(ns clj-predict.orbit
  "Functions for satellite orbit operations."
  (:require [clj-predict.coordinates :as coord]
            [clj-predict.properties :as props]
            [clj-predict.time :as time]))

(defn orbit-state
  [args]
  (merge {:body :earth
          :time (time/now)}
         (apply hash-map args)))

;;;; Orbit Properties ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn period
  [semi-major-axis & args]
  (let [s (orbit-state args)
        a  semi-major-axis
        mu (:mu (props/body (:body s)))]
    (/ (* 2 Math/PI (Math/sqrt (/ (* a a a) mu))) 86400)))

(defn revolutions
  [semi-major-axis & args]
  (/ 1 (apply (partial period semi-major-axis) args)))

;;;; Keplerian Elements ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mech-energy
  [r v & args]
  (let [s (orbit-state args)
        body (props/body (:body s))
        V-sq (Math/pow (coord/mag v) 2)
        R (coord/mag r)
        mu (:mu body)]
    (- (/ V-sq 2) (/ mu R))))

(defn semi-major-axis
  [r v & args]
  (let [s (orbit-state args)
        mu (:mu (props/body (:body s)))
        en (apply (partial mech-energy r v) args)]
    (- (/ mu (* 2 en)))))

(defn ecc-vector
  [r v & args]
  (let [s (orbit-state args)
        V-sq (Math/pow (coord/mag v) 2)
        R (coord/mag r)
        mu (:mu (props/body (:body s)))
        a (map * (repeat (- (/ V-sq mu) (/ 1 R))) r)
        b (map * (repeat (/ (coord/dot r v) mu)) v)]
    (map - a b)))

(defn angular-momentum
  [r v]
  (coord/cross r v))

(defn inclination
  [r v]
  (let [H (angular-momentum r v)
        hk (last H)
        h (coord/mag H)]
    (coord/rad->deg (Math/acos (/ hk h)))))

(defn node-vector
  [r v]
  (let [K [0 0 1]
        H (angular-momentum r v)]
    (coord/cross K H)))

(defn right-ascension
  [r v]
  (let [N (node-vector r v)
        ni (first N)
        nj (second N)
        n (coord/mag N)
        i (inclination r v)]
    (if (or (zero? i) (>= i 180)) 0.0
      (let [raan (coord/rad->deg (Math/acos (/ ni n)))]
        (if (neg? nj) (- 360 raan) raan)))))

(defn argument-of-perigee
  [r v & args]
  (let [N (node-vector r v)
        E (apply (partial ecc-vector r v) args)
        n (coord/mag N)
        e (coord/mag E)
        ei (first E)
        ej (second E)
        ek (last E)
        i (inclination r v)
        a (coord/dot N E)
        b (* n e)]
    (cond
      (zero? e) 0.0
      (or (zero? i) (>= i 180)) (coord/rad->deg (Math/atan2 ej ei))
      :else (let [aop (coord/rad->deg (Math/acos (/ a b)))]
              (if (neg? ek) (- 360 aop) aop)))))

(defn argument-of-latitude
  [r v]
  (let [n (node-vector r v)
        n-mag (coord/mag n)
        r-mag (coord/mag r)
        n-dot-v (coord/dot n v)
        a (coord/dot n r)
        b (* n-mag r-mag)
        aol (coord/rad->deg (Math/acos (/ a b)))]
    (if (pos? n-dot-v) (- 360 aol) aol)))

(defn true-longitude
  [r v]
  (let [ri (first r)
        vi (first v)
        rm (coord/mag r)
        tl (coord/rad->deg (Math/acos (/ ri rm)))]
    (if (pos? vi) (- 360 tl) tl)))

(defn true-anomaly
  [r v & args]
  (let [e-vec (apply (partial ecc-vector r v) args)
        e-mag (coord/mag e-vec)
        r-mag (coord/mag r)
        r-dot-v (coord/dot r v)
        i (inclination r v)
        arg-lat? (zero? e-mag)
        tru-lon? (and (zero? e-mag) (or (zero? i) (>= i 180)))
        a (coord/dot e-vec r)
        b (* e-mag r-mag)]
    (cond
      tru-lon? (true-longitude r v)
      arg-lat? (argument-of-latitude r v)
      :else (let [ta (coord/rad->deg (Math/acos (/ a b)))]
              (if (neg? r-dot-v) (- 360 ta) ta)))))

(defn rv->kepler
  [r v & args]
  (let [s (orbit-state args)]
    {:b (:body s) :t (:time s)
     :a (apply (partial semi-major-axis r v) args)
     :e (coord/mag (apply (partial ecc-vector r v) args))
     :i (inclination r v)
     :o (right-ascension r v)
     :w (apply (partial argument-of-perigee r v) args)
     :v (apply (partial true-anomaly r v) args)}))
