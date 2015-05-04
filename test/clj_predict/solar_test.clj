(ns clj-predict.solar-test
  (:use clojure.test clj-predict.solar))

(defn date-format
  [s]
  (let [sdf (doto (java.text.SimpleDateFormat. "yy-DDD HH:mm")
              (.setTimeZone (java.util.TimeZone/getTimeZone "GMT")))]
    (.parse sdf s)))

(deftest solar-parameters
  (is (= (:mean-distance solar-properties) 149597870700))
  (is (= (:eccentricity solar-properties) 0.01617))
  (is (= (:orbital-period solar-properties) 365.25636))
  (is (= (:declination solar-properties) 23.439281)))

(def spring-equinox  (date-format "15-079 22:45"))
(def summer-solstice (date-format "15-172 16:38"))
(def fall-equinox    (date-format "15-266 08:20"))
(def winter-solstice (date-format "15-356 04:38"))

(def time-range
  (map date-format (for [m (range 0 60 5)] (format "15-080 00:%02d" m))))

(deftest solar-location
  (let [v-fn #(vector (int (:lat %)) (int (:lon %)) (int (/ (:alt %) 1000)))]
    (is (= (v-fn (solar-position spring-equinox))
           [0 -159 148728666]))
    (is (= (v-fn (solar-position summer-solstice))
           [23 -69 151876021]))
    (is (= (v-fn (solar-position fall-equinox))
           [0 52 150318906]))
    (is (= (v-fn (solar-position winter-solstice))
           [-23 110 147307778]))
    (is (= (vec (map v-fn (map #(solar-position %) time-range)))
           [[0 -178 148730689] [0 -179 148730824] [0 179 148730959]
            [0 178 148731094]  [0 176 148731228]  [0 175 148731363]
            [0 174 148731498]  [0 173 148731633]  [0 171 148731768]
            [0 170 148731903]  [0 169 148732038]  [0 168 148732173]]))))
