(ns clj-predict.orbit
  "Functions for satellite orbit operations."
  (:require [clj-predict.coordinates :as coord]
            [clj-predict.properties :as props]
            [clj-predict.time :as time]))

;;;; Orbit Properties ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn period
  ([semi-major-axis]
    (period semi-major-axis (props/celestial-body)))
  ([semi-major-axis body]
    (let [a (double semi-major-axis)
          b (props/celestial-map body)
          mu (:mu b)]
      (/ (* 2 Math/PI (Math/sqrt (/ (* a a a) mu))) 86400))))

(defn revolutions
  ([semi-major-axis]
    (revolutions semi-major-axis (props/celestial-body)))
  ([semi-major-axis body]
    (double (/ 1 (period semi-major-axis body)))))

;;;; Keplerian Elements ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mechanical-energy
  ([{:keys [r v] :as state-vectors}]
    (mechanical-energy state-vectors (props/celestial-body)))
  ([{:keys [r v] :as state-vectors} body]
    (let [V-sq (Math/pow (coord/magnitude v) 2)
          R (coord/magnitude r)
          mu (:mu (props/celestial-map body))]
      (- (/ V-sq 2) (/ mu R)))))

(defn semi-major-axis
  ([{:keys [r v] :as state-vectors}]
    (semi-major-axis state-vectors (props/celestial-body)))
  ([{:keys [r v] :as state-vectors} body]
    (let [mu (:mu (props/celestial-map body))
          en (mechanical-energy state-vectors body)]
      (- (/ mu (* 2 en))))))

(defn eccentricity-vector
  ([{:keys [r v] :as state-vectors}]
    (eccentricity-vector state-vectors (props/celestial-body)))
  ([{:keys [r v] :as state-vectors} body]
    (let [V-sq (Math/pow (coord/magnitude v) 2)
          R (coord/magnitude r)
          mu (:mu (props/celestial-map body))
          a (map * (repeat (- (/ V-sq mu) (/ 1 R))) r)
          b (map * (repeat (/ (coord/dot r v) mu)) v)]
      (map - a b))))

(defn angular-momentum
  [{:keys [r v] :as state-vectors}]
  (coord/cross r v))

(defn inclination
  [{:keys [r v] :as state-vectors}]
  (let [H (angular-momentum state-vectors)
        hk (last H)
        h (coord/magnitude H)]
    (coord/rad->deg (Math/acos (/ hk h)))))

(defn node-vector
  [{:keys [r v] :as state-vectors}]
  (let [K [0 0 1]
        H (angular-momentum state-vectors)]
    (coord/cross K H)))

(defn right-ascension
  [{:keys [r v] :as state-vectors}]
  (let [N (node-vector state-vectors)
        ni (first N)
        nj (second N)
        n (coord/magnitude N)
        i (inclination state-vectors)]
    (if (or (zero? i) (>= i 180)) 0.0
      (let [raan (coord/rad->deg (Math/acos (/ ni n)))]
        (if (neg? nj) (- 360 raan) raan)))))

(defn argument-of-perigee
  ([{:keys [r v] :as state-vectors}]
    (argument-of-perigee state-vectors (props/celestial-body)))
  ([{:keys [r v] :as state-vectors} body]
    (let [N (node-vector state-vectors)
          E (eccentricity-vector state-vectors body)
          n (coord/magnitude N)
          e (coord/magnitude E)
          ei (first E)
          ej (second E)
          ek (last E)
          i (inclination state-vectors)
          a (coord/dot N E)
          b (* n e)]
      (cond
        (zero? e) 0.0
        (or (zero? i) (>= i 180)) (coord/rad->deg (Math/atan2 ej ei))
        :else (let [aop (coord/rad->deg (Math/acos (/ a b)))]
                (if (neg? ek) (- 360 aop) aop))))))

(defn argument-of-latitude
  [{:keys [r v] :as state-vectors}]
  (let [n (node-vector state-vectors)
        n-mag (coord/magnitude n)
        r-mag (coord/magnitude r)
        n-dot-v (coord/dot n v)
        a (coord/dot n r)
        b (* n-mag r-mag)
        aol (coord/rad->deg (Math/acos (/ a b)))]
    (if (pos? n-dot-v) (- 360 aol) aol)))

(defn true-longitude
  [{:keys [r v] :as state-vectors}]
  (let [ri (first r)
        vi (first v)
        rm (coord/magnitude r)
        tl (coord/rad->deg (Math/acos (/ ri rm)))]
    (if (pos? vi) (- 360 tl) tl)))

(defn true-anomaly
  ([{:keys [r v] :as state-vectors}]
    (true-anomaly state-vectors (props/celestial-body)))
  ([{:keys [r v] :as state-vectors} body]
    (let [e-vec (eccentricity-vector state-vectors body)
          e-mag (coord/magnitude e-vec)
          r-mag (coord/magnitude r)
          r-dot-v (coord/dot r v)
          i (inclination state-vectors)
          arg-lat? (zero? e-mag)
          tru-lon? (and (zero? e-mag) (or (zero? i) (>= i 180)))
          a (coord/dot e-vec r)
          b (* e-mag r-mag)]
      (cond
        tru-lon? (true-longitude state-vectors)
        arg-lat? (argument-of-latitude state-vectors)
        :else (let [ta (coord/rad->deg (Math/acos (/ a b)))]
                (if (neg? r-dot-v) (- 360 ta) ta))))))

(defn rv->kepler
  ([{:keys [r v t] :as state-vectors}]
    (rv->kepler state-vectors (props/celestial-body)))
  ([{:keys [r v t] :as state-vectors} body]
    {:b body :t t
     :a (semi-major-axis state-vectors body)
     :e (coord/magnitude (eccentricity-vector state-vectors body))
     :i (inclination state-vectors)
     :o (right-ascension state-vectors)
     :w (argument-of-perigee state-vectors body)
     :v (true-anomaly state-vectors body)}))
