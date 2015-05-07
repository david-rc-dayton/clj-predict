(ns clj-predict.time
  "Helper functions for dealing with time.")

(def pattern-default (atom "yy-DDD HH:mm:ss"))

(defn pattern-default!
  [s]
  (reset! pattern-default s))

(defn date-format
  ([]
    (date-format (java.util.Date.)))
  ([date]
    (let [sd (doto (java.text.SimpleDateFormat. @pattern-default)
               (.setTimeZone (java.util.TimeZone/getTimeZone "UTC")))]
      (condp = (type date)
        java.util.Date (.format sd date)
        java.lang.String (.parse sd date)))))

(defn date->epoch-days
  ([]
    (date->epoch-days (java.util.Date.)))
  ([date]
    (double (/ (.getTime date) 1000 60 60 24))))

(def j2000 (date->epoch-days (date-format "00-001 12:00:00")))

(defn gmst
  ([]
    (gmst (java.util.Date.)))
  ([date]
    (let [delta (- (date->epoch-days date) j2000)]
      (* (/ Math/PI 180)
         (-> (+ 280.46061837 (* 360.98564736629 delta)) (mod 360))))))

(defn local-sidereal
  ([]
    (local-sidereal (java.util.Date.)))
  ([date]
    (let [day (date->epoch-days date)
          d-int (int day)
          d-frac (- day d-int)
          du (- d-int j2000)
          tu (/ du 36525)
          we (/ 1 0.99726958)
          ds (* we d-frac 60 60 24)
          gmst (mod (- (+ 24110.54841 (* 8640184.812866 tu)
                          (Math/pow (* 0.093104 tu) 2))
                       (Math/pow (* 6.2e-6 tu) 3))
                    (* 60 60 24))]
      (* 2 Math/PI (/ (+ gmst ds) 86400.0)))))
