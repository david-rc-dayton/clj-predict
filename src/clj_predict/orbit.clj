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
          b (props/celestial-body body)
          mu (:mu b)]
      (/ (* 2 Math/PI (Math/sqrt (/ (* a a a) mu))) 86400))))

(defn revolutions
  ([semi-major-axis]
    (revolutions semi-major-axis (props/celestial-body)))
  ([semi-major-axis body]
    (double (/ 1 (period semi-major-axis body)))))

(defn mechanical-energy
  ([state-vector]
    (mechanical-energy state-vector (props/celestial-body)))
  ([state-vector body]
    (let [b (props/celestial-body body)
          r (coord/magnitude (:r state-vector))
          v (coord/magnitude (:v state-vector))
          v-sq (* v v)
          mu (:mu b)]
      (- (/ v-sq 2) (/ mu r)))))

;;;; Keplerian Elements ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn eccentricity
  ([state-vector]
    (eccentricity state-vector (props/celestial-body)))
  ([state-vector body]
    (let [b (props/celestial-body body)
          r (:r state-vector)
          v (:v state-vector)
          h (coord/cross r v)
          mu (:mu b)
          s (map #(/ % mu) (coord/cross v h))
          e (map #(/ % (coord/magnitude r)) r)]
      (map #(- %1 %2) s e))))

(defn semi-major-axis
  ([state-vector]
    (semi-major-axis state-vector (props/celestial-body)))
  ([state-vector body]
    (let [b (props/celestial-body body)
          mu (:mu b)
          ep (mechanical-energy state-vector body)]
      (- (/ mu (* 2 ep))))))

(defn inclination
  [state-vector]
  (let [r (:r state-vector)
        v (:v state-vector)
        h (coord/cross r v)]
    (coord/rad->deg (Math/acos (/ (nth h 2) (coord/magnitude h))))))

(defn ascending-node
  [state-vector]
  (let [r (:r state-vector)
        v (:v state-vector)
        k [0 0 1]
        h (coord/cross r v)
        n (coord/cross k h)
        o (Math/acos (/ (nth n 0) (coord/magnitude n)))]
    (coord/rad->deg (if (>= (nth n 1) 0) o (- (* 2 Math/PI) o)))))

(defn argument-periapsis
  ([state-vector]
    (argument-periapsis state-vector (props/celestial-body)))
  ([state-vector body]
    (let [r (:r state-vector)
          v (:v state-vector)
          k [0 0 1]
          h (coord/cross r v)
          n (coord/cross k h)
          e (eccentricity state-vector body)
          w (Math/acos (/ (coord/dot n e)
                          (* (coord/magnitude n) (coord/magnitude e))))]
      (coord/rad->deg (if (< (nth e 2) 0) (- (* 2 Math/PI) w) w)))))

(defn true-anomaly
  ([state-vector]
    (true-anomaly state-vector (props/celestial-body)))
  ([state-vector body]
    (let [e (eccentricity state-vector body)
          r (:r state-vector)
          v (:v state-vector)
          m (coord/dot r v)
          an (Math/acos (/ (coord/dot e r)
                           (* (coord/magnitude e) (coord/magnitude r))))]
      (coord/rad->deg (if (neg? m) (- (* 2 Math/PI) an) an)))))

(defn eccentric-anomaly
  ([state-vector]
    (eccentric-anomaly state-vector (props/celestial-body)))
  ([state-vector body]
    (let [e (coord/magnitude (eccentricity state-vector body))
          n (coord/deg->rad (true-anomaly state-vector body))
          ea (Math/acos (/ (+ e (Math/cos n))
                          (+ 1 (* e (Math/cos n)))))]
      (coord/rad->deg (if (>= n Math/PI) (- (* 2 Math/PI) ea) ea)))))

(defn mean-anomaly
  ([state-vector]
    (mean-anomaly state-vector (props/celestial-body)))
  ([state-vector body]
    (let [e (coord/magnitude (eccentricity state-vector body))
          ea (coord/deg->rad (eccentric-anomaly state-vector body))]
      (coord/rad->deg (- ea (* e (Math/sin ea)))))))

(defn rv->kepler
  ([state-vector]
    (rv->kepler state-vector (props/celestial-body)))
  ([state-vector body]
    {:t (:t state-vector)
     :e (coord/magnitude (eccentricity state-vector body))
     :a (semi-major-axis state-vector body)
     :i (inclination state-vector)
     :o (ascending-node state-vector)
     :w (argument-periapsis state-vector body)
     :m (mean-anomaly state-vector body)}))