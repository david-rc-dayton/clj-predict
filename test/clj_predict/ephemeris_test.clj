(ns clj-predict.ephemeris-test
  (:require [clj-predict.time :as t]
            [clj-predict.ephemeris :refer :all]
            [clojure.test :refer :all]))

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

(def date-1 (t/date-parse "15-080 00:00:00"))
(def date-2 (t/date-parse "15-072 06:30:15"))
(def date-3 (t/date-parse "15-094 13:00:30"))

(deftest tle-validity
  (is (true? (valid-tle? iss-tle)))
  (is (true? (valid-tle? good-tle)))
  (is (false? (valid-tle? bad-tle-1)))
  (is (false? (valid-tle? bad-tle-2)))
  (is (false? (valid-tle? bad-tle-3))))

(deftest propagate-sgp4
  (is (= (sgp4 iss-tle date-1)
         [10.890905513566647 -4.931594881895251 403.29369648476677]))
  (is (= (sgp4 iss-tle date-2)
         [-50.320633679307576 -136.5303906855194 425.3712974694081]))
  (is (= (sgp4 iss-tle date-3)
         [22.428015399147736 -134.73324827808446 394.8746860965266]))
  (is (= (sgp4 good-tle date-1)
         [6.109987468271647 -0.5179502896249346 35795.44244126648]))
  (is (= (sgp4 good-tle date-2)
         [-11.694463122106589 -1.8599962733172788 35788.55520517166]))
  (is (= (sgp4 good-tle date-3)
         [0.4804782469300983 -1.0762411733389285 35775.3778418229])))
