(ns clj-predict.solar)

(def ^{:private true} solar-properties
  {:mean-distance 149597870700
   :eccentricity 0.0161700
   :orbital-period 365.25636
   :declination 23.439281})

(defn ^{:private true} day-of-year
  "Return the number of days elapsed since the beginning of the year for input
   argument `date`. Returns a floating point representation of elapsed time in
   the UTC timezone."
  [^java.util.Date date]
  (let [cal (doto (java.util.Calendar/getInstance
                    (java.util.TimeZone/getTimeZone "UTC"))
              (.setTime date))
        d (.get cal java.util.Calendar/DAY_OF_YEAR)
        h (.get cal java.util.Calendar/HOUR_OF_DAY)
        m (.get cal java.util.Calendar/MINUTE)
        s (.get cal java.util.Calendar/SECOND)
        f (/ (+ (* h 3600) (* m 60) s) 86400)]
    (double (+ d f))))

(defn ^{:private true} solar-latitude
  "Return the latitude of the sun, in degrees, for the input `date`."
  [^java.util.Date date]
  (let [d (+ (day-of-year date)
             (+ (* (:orbital-period solar-properties) 3/4) 10))
        r (/ (* 2 Math/PI) (:orbital-period solar-properties))
        t (:declination solar-properties)
        rad->deg #(* % (/ 180 Math/PI))]
    (* t (Math/sin (* r d)))))

(defn ^{:private true} solar-longitude
  "Return the longitude of the sun, in degrees, for input `date`."
  [^java.util.Date date]
  (let [d (day-of-year date)
        p (:orbital-period solar-properties)
        b (/ (* 2 Math/PI (- (int d) (- (* p 1/4) 10))) p)
        eot (- (* 9.87 (Math/sin (* 2 b))) (* 7.53 (Math/cos b))
               (* 1.5 (Math/sin b)))
        t (+ (- 360 (mod (* (+ (- d (int d)) (/ eot 1440)) 360) 360)) 180)]
    (cond
      (<= t -180) (+ t 360)
      (> t 180) (- t 360)
      :else t)))

(defn ^{:private true} solar-altitude
  "Calculate the distance of the Sun from Earth in meters for input argument
   `date`."
  [^java.util.Date date]
  (let [dn (day-of-year date)
        ro (:mean-distance solar-properties)
        ec (:eccentricity solar-properties)
        T (:orbital-period solar-properties)
        t (+ (* T 1/4) 10)
        pi (Math/PI)]
    (* ro (+ 1 (* ec (Math/sin (/ (* 2 pi (- dn t)) T)))))))

(defn solar-position
  "Return a map containing the `:latitude` and `:longitude` of the sun in
   degrees, as well as the `:altitude` in meters for the input `date`. If a 
   date object is not provided, current system time will be used."
  ([]
    (solar-position (java.util.Date.)))
  ([^java.util.Date date]
    {:latitude (solar-latitude date) 
     :longitude (solar-longitude date)
     :altitude (solar-altitude date)}))
