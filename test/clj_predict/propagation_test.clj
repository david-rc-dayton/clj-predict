(ns clj-predict.propagation-test
  (:use clojure.test clj-predict.propagation))

(defn date-format
  [s]
  (let [sdf (doto (java.text.SimpleDateFormat. "yy-DDD HH:mm")
              (.setTimeZone (java.util.TimeZone/getTimeZone "GMT")))]
    (.parse sdf s)))

(def iss-tle
  ["ISS (ZARYA)"
   "1 25544U 98067A   15079.11087502  .00036184  00000-0  53177-3 0  9994"
   "2 25544  51.6462 168.9967 0007132 120.3682 312.6240 15.55096738934178"])

(def good-tle
  ["SKYNET 4C"
   "1 20776U 90079A   15077.07355926 -.00000044  00000-0  00000+0 0  9992"
   "2 20776  13.2556  24.7266 0002486 334.4102 201.4692  1.00273775 89744"])

(def bad-tle-1
  [""
   "1 20776U 90079A   15077.07355926 -.00000044  00000-0  00000+0 0  9992"
   "2 20776  13.2556  24.7266 0002486 334.4102 201.4692  1.00273775 89744"])

(def bad-tle-2
  ["SKYNET 4C"
   "1 20776U 90089A   15077.07355926 -.00000044  00000-0  00000+0 0  9992"
   "2 20776  13.2556  24.7266 0002486 334.4102 201.4692  1.00273775 89744"])

(def bad-tle-3
  ["SKYNET 4C"
   "1 20776U 90079A   15077.07355926 -.00000044  00000-0  00000+0 0  9992"
   "2 20776  13.2556  24.7266 0002486 334.4102 203.4692  1.00273775 89744"])

(def date-1 (date-format "15-080 00:00"))
(def date-2 (date-format "15-070 00:00"))
(def date-3 (date-format "15-090 00:00"))

(def time-range
  (map date-format (for [m (range 0 60 5)] (format "15-080 00:%02d" m))))

(deftest tle-validity
  (is (true? (valid-tle? iss-tle)))
  (is (true? (valid-tle? good-tle)))
  (is (false? (valid-tle? bad-tle-1)))
  (is (false? (valid-tle? bad-tle-2)))
  (is (false? (valid-tle? bad-tle-3))))

(deftest propagate-ephemeris
  (let [v-fn #(vector (int (:lat %)) (int (:lon %)) (int (/ (:alt %) 1000)))]
    (is (= (v-fn (propagate iss-tle date-1))
           [10 -4 403]))
    (is (= (v-fn (propagate iss-tle date-2))
           [12 -143 402]))
    (is (= (v-fn (propagate iss-tle date-3))
           [-47 166 415]))
    (is (= (v-fn (propagate good-tle date-1))
           [6 0 35795]))
    (is (= (v-fn (propagate good-tle date-2))
           [8 0 35795]))
    (is (= (v-fn (propagate good-tle date-3))
           [3 0 35795]))
    (is (= (vec (map v-fn (map #(propagate iss-tle %) time-range)))
           [[10 -4 403]  [25 7 402]    [38 23 403]
            [48 45 403]  [51 74 403]   [47 102 401]
            [36 123 399] [22 137 397]  [8 149 398]
            [-7 160 401] [-22 172 407] [-35 -172 414]]))
    (is (= (vec (map v-fn (map #(propagate good-tle %) time-range)))
           [[6 0 35795]  [5 0 35795]   [5 0 35795]
            [5 0 35795]  [5 0 35795]   [4 0 35795]
            [4 0 35795]  [4 0 35795]   [3 0 35795]
            [3 0 35795]  [3 0 35795]   [3 0 35795]]))))
