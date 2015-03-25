(ns clj-predict.coordinates-test
  (:use clojure.test clj-predict.coordinates))

(def tolerance 1e-5)

(def org {:lat 0 :lon 0 :alt 0})

(def points 
  [{:lat 45 :lon 0} {:lat 45 :lon 45} {:lat 0 :lon 45} {:lat -45 :lon 45}
   {:lat -45 :lon 0} {:lat -45 :lon -45} {:lat 0 :lon -45} {:lat 45 :lon -45}])

(def altitudes [{:alt 160000} {:alt 2000000} {:alt 35786000}])

(def locations (for [p points a altitudes] (merge p a)))

(def geo-perm
  (for [lat (range -90 90 15)
        lon (range -180 180 15)
        alt (range 160000 35786000 2968833)]
    {:lat lat :lon lon :alt alt}))

(def ecf-perm
  (let [box (range 6378137 42164137 3513678)]
    (for [xf box yf box zf box]
      {:xf xf :yf yf :zf zf})))

(def bodies 
  {:sun  {:dist 149597870700 :diam 1392000000}
   :moon {:dist 384400000    :diam 3474200}
   :geo  {:dist 35786000     :diam 12742000}})

(deftest wgs-ellipsoid
  (is (= (:semi-major-axis wgs84) 6378137))
  (is (= (:semi-minor-axis wgs84) 6356752.314245179))
  (is (= (:mean-radius wgs84)     6371008.771415059))
  (is (= (:coeff-flat wgs84)      0.0033528106647474805))
  (is (= (:ecc-squared wgs84)     0.0066943799901413165)))

(deftest geocentric-radius
  (let [lats (range -90 90 45)]
    (is (= (vec (map geo-radius (map #(hash-map :lat %) lats)))
           [6356752.314245179 6367489.543863465 
            6378137.0         6367489.543863465]))))

(deftest angular-distance
  (is (= (vec (map #(int (adist-haversine org %)) points))
         [45 60 45 60 45 60 45 60]))
  (is (= (vec (map #(int (adist-cosine org %)) points))
         [45 59 45 59 45 59 45 59]))
  (is (= (vec (map #(int (adist-equirect org %)) points)) 
         [45 61 45 61 45 61 45 61])))

(deftest distance-to-horizon
  (is (= (vec (map #(int (horizon %)) locations))
         [12 40 81 12 40 81 12 40 81 12 40 81
          12 40 81 12 40 81 12 40 81 12 40 81])))

(deftest angular-diameter
  (is (= (adiam-sphere (:dist (:sun bodies)) (:diam (:sun bodies)))
         0.5331360161562749))
  (is (= (adiam-sphere (:dist (:moon bodies)) (:diam (:moon bodies)))
         0.5178399445635666))
  (is (= (adiam-sphere (:dist (:geo bodies)) (:diam (:geo bodies)))
         20.51012765215461))
  (is (= (adiam-disc (:dist (:sun bodies)) (:diam (:sun bodies)))
         0.5331302462100577))
  (is (= (adiam-disc (:dist (:moon bodies)) (:diam (:moon bodies)))
         0.517834657132248))
  (is (= (adiam-disc (:dist (:geo bodies)) (:diam (:geo bodies)))
         20.189268988276787)))

(deftest satellite-view
  (let [eq-points (for [alt altitudes] (merge {:lat 0 :lon 0} alt))
        out-eq [[[12 0]    [8 9]     [0 12]    [-8 9]
                 [-12 0]   [-8 -9]   [0 -12]   [8 -9]]
                [[40 0]    [27 31]   [0 40]    [-27 31]
                 [-40 0]   [-27 -31] [0 -40]   [27 -31]]
                [[81 0]    [44 77]   [0 81]    [-44 77]
                 [-81 0]   [-44 -77] [0 -81]   [44 -77]]]
        v-fn (fn [x] (vector (int (:lat x)) (int (:lon x))))]
    (is (= (count (filter #(surface-visible? % org adist-haversine) geo-perm))
           1005))
    (is (= (count (filter #(surface-visible? % org adist-cosine) geo-perm))
           1005))
    (is (= (count (filter #(surface-visible? % org adist-equirect) geo-perm))
           953))
    (is (= (vec (map #(vec (map v-fn (horizon-outline % 45))) eq-points))
           out-eq))))

(deftest coordinate-convert
  (let [ecf-pts (map geodetic->ecf geo-perm)
        chk-pts (map ecf->geodetic ecf-pts)
        geo-check #(and (<= (Math/abs (- (Math/abs (:lat %1)) 
                                         (Math/abs (:lat %2)))) tolerance)
                        (<= (Math/abs (- (Math/abs (:lon %1))
                                         (Math/abs (:lon %2)))) tolerance)
                        (<= (Math/abs (- (Math/abs (:alt %1))
                                         (Math/abs (:alt %2)))) tolerance))]
    (is (every? true? (map geo-check geo-perm chk-pts))))
  (let [geo-pts (map ecf->geodetic ecf-perm)
        chk-pts (map geodetic->ecf geo-pts)
        ecf-check #(and (<= (Math/abs (- (Math/abs (:xf %1)) 
                                         (Math/abs (:xf %2)))) tolerance)
                        (<= (Math/abs (- (Math/abs (:yf %1))
                                         (Math/abs (:yf %2)))) tolerance)
                        (<= (Math/abs (- (Math/abs (:zf %1))
                                         (Math/abs (:zf %2)))) tolerance))]
    (is (every? true? (map ecf-check ecf-perm chk-pts)))))

(deftest antenna-pointing
  (let [v-fn #(vector (int (:az %)) (int (:el %)) (int (/ (:rng %) 1000)))]
    (is (= (vec (map v-fn (filter :vis? (map #(look-angle org %) locations))))
           [[0 38 37910]   [35 21 39352]  [90 38 37923]  [144 21 39352]
            [180 38 37910] [215 21 39352] [270 38 37923] [324 21 39352]]))))

(deftest aspect
  (let [org-pt-one {:lat 14 :lon -27 :alt 35786000}
        fst-pt-one {:lat 0  :lon 0   :alt 25}
        snd-pt-one {:lat 20 :lon 25  :alt 2000}
        org-pt-two {:lat -10 :lon 35 :alt 12000}
        fst-pt-two {:lat 12  :lon 170   :alt 2500}
        snd-pt-two {:lat -45 :lon 25  :alt 800}]
    (is (= (aspect-angle org-pt-one fst-pt-one snd-pt-one)
           4.70726464352193))
    (is (= (aspect-angle org-pt-two fst-pt-two snd-pt-two)
           79.9920611776812))))

