(ns clj-predict.coverage-test
  (:use clojure.test clj-predict.coverage))

(deftest range-float
  (is (= (rangef  0  2  0.5) [ 0.0  0.5  1.0  1.5]))
  (is (= (rangef  0 -2 -0.5) [ 0.0 -0.5 -1.0 -1.5]))
  (is (= (rangef -1  1  0.5) [-1.0 -0.5  0.0  0.5]))
  (is (= (rangef  1 -1 -0.5) [ 1.0  0.5  0.0 -0.5])))

(deftest rad-coverage
  (let [phis [half-pi (- half-pi)]
        lams [pi (- pi)]
        expected? (partial = 1.5707963267948966)]
    (is (every? expected?
                (for [p phis l lams] (cov-cosine 0 0 p l))))
    (is (every? expected?
                (for [p phis l lams] (cov-haversine 0 0 p l))))))

(deftest view-check
  (let [geo-cos (view-fn :cosine    {:lat 0 :lon 0 :alt 35786000})
        geo-hav (view-fn :haversine {:lat 0 :lon 0 :alt 35786000})
        geo-expected? (complement zero?)
        iss-cos (view-fn :cosine    {:lat 0 :lon 0 :alt 398500})
        iss-hav (view-fn :haversine {:lat 0 :lon 0 :alt 398500})
        iss-expected? zero?
        phis (map (partial * 0.25) [half-pi (- half-pi)])
        lams (map (partial * 0.25) [pi (- pi)])]
    (is (every? geo-expected? (for [p phis l lams] (geo-cos p l))))
    (is (every? geo-expected? (for [p phis l lams] (geo-hav p l))))
    (is (every? iss-expected? (for [p phis l lams] (iss-cos p l))))
    (is (every? iss-expected? (for [p phis l lams] (iss-hav p l))))))

(deftest matrices
  (let [blank-dim [3  3]
        cover-dim [10 5]
        cn-loc {:lat   0 :lon   0  :alt 35786000}
        tl-loc {:lat  45 :lon -90  :alt   398000}
        tr-loc {:lat  45 :lon  90  :alt   398000}
        bl-loc {:lat -45 :lon -90  :alt   398000}
        br-loc {:lat -45 :lon  90  :alt   398000}
        ev-loc [cn-loc tl-loc tr-loc br-loc bl-loc]]
    (is (= (blank-matrix blank-dim)
           [[1.5707963267948966
             [-3.141592653589793 -1.0471975511965979 1.0471975511965974]]
            [0.5235987755982989
             [-3.141592653589793 -1.0471975511965979 1.0471975511965974]]
            [-0.5235987755982987
             [-3.141592653589793 -1.0471975511965979 1.0471975511965974]]]))
    (is (= (coverage-matrix :cosine cn-loc cover-dim)
           [[0 0 0 0 0 0 0 0 0 0]
            [0 0 0 1 1 1 1 1 0 0]
            [0 0 0 1 1 1 1 1 0 0]
            [0 0 0 1 1 1 1 1 0 0]
            [0 0 0 1 1 1 1 1 0 0]]))
    (is (= (coverage-matrix :cosine tl-loc cover-dim)
           [[0 0 0 0 0 0 0 0 0 0]
            [0 0 1 1 0 0 0 0 0 0]
            [0 0 0 0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0 0 0 0]]))
    (is (= (coverage-matrix :cosine tr-loc cover-dim)
           [[0 0 0 0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0 1 1 0]
            [0 0 0 0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0 0 0 0]]))
    (is (= (coverage-matrix :cosine bl-loc cover-dim)
           [[0 0 0 0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0 0 0 0]
            [0 0 1 1 0 0 0 0 0 0]]))
    (is (= (coverage-matrix :cosine br-loc cover-dim)
           [[0 0 0 0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0 0 0 0]
            [0 0 0 0 0 0 0 1 1 0]]))
    (is (= (coverage-matrix :cosine ev-loc cover-dim)
           [[0 0 0 0 0 0 0 0 0 0]
            [0 0 1 2 1 1 1 2 1 0]
            [0 0 0 1 1 1 1 1 0 0]
            [0 0 0 1 1 1 1 1 0 0]
            [0 0 1 2 1 1 1 2 1 0]]))
    (is (= (coverage-matrix :cosine    cn-loc cover-dim)
           (coverage-matrix :haversine cn-loc cover-dim)))
    (is (= (coverage-matrix :cosine    tl-loc cover-dim)
           (coverage-matrix :haversine tl-loc cover-dim)))
    (is (= (coverage-matrix :cosine    tr-loc cover-dim)
           (coverage-matrix :haversine tr-loc cover-dim)))
    (is (= (coverage-matrix :cosine    bl-loc cover-dim)
           (coverage-matrix :haversine bl-loc cover-dim)))
    (is (= (coverage-matrix :cosine    br-loc cover-dim)
           (coverage-matrix :haversine br-loc cover-dim)))
    (is (= (coverage-matrix :cosine    ev-loc cover-dim)
           (coverage-matrix :haversine ev-loc cover-dim)))))
