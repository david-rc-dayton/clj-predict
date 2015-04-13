(ns clj-predict.coverage
  "Matrix generation functions for satellite coverage analysis.")

(def pi (Math/PI))
(def two-pi (* 2 ^double pi))
(def half-pi (/ ^double pi 2))

(def ascii-legend
  {0 ". " 1 "- " 2 "= "
   3 "% " 4 "+ " 5 "V "
   :default "@ "})

(defn doall-recur [s]
  (if (seq? s)
    (doall (map doall-recur s))
    s))

(defn print-matrix
  [m]
  (let [replace-fn #(or (get ascii-legend %) (:default ascii-legend))
        str-fn #(apply str (map replace-fn %))]
    (dorun (map #(println (str-fn %)) m))))

(defn rangef
  [^double start ^double end ^double step]
  (map #(+ start (* ^double % step))
       (range 0 (/ (Math/abs (- end start))
                   (Math/abs step)) 1)))

(defn cov-cosine
  [^double phi-1 ^double lam-1 ^double phi-2 ^double lam-2]
  (let [delta-phi-two (-> (Math/sin (/ (- phi-2 phi-1) 2)) (Math/pow 2))
        delta-lam-two (-> (Math/sin (/ (- lam-2 lam-1) 2)) (Math/pow 2))
        a (+ delta-phi-two (* (Math/cos phi-1) (Math/cos phi-2) delta-lam-two))]
    (* 2 (Math/atan2 (Math/sqrt a) (Math/sqrt (- 1 a))))))

(def cov-methods
  {:cosine cov-cosine})

(defn view-fn
  [method {:keys [lat lon alt]}]
  (let [sat-lat (* ^double lat (/ ^double pi 180))
        sat-lon (* ^double lon (/ ^double pi 180))
        view-limit (Math/acos (/ 6371000 (+ 6371000 ^double alt)))
        view-method (get cov-methods method)]
    (fn [gnd-lat gnd-lon]
      (if (<= ^double (view-method sat-lat sat-lon gnd-lat gnd-lon)
              view-limit) 1 0))))

(defn blank-matrix
  "Generate a blank coverage matrix. Takes the argument:

   > `dimensions` - a vector of the `width` & `height` for the desired matrix

   Returns a matrix containing coordinates of the form
   `[latitude [longitudes]]`, in radians."
  [[width height]]
  (let [step-width (/ ^double two-pi ^int width)
        step-height (/ ^double pi ^long height)
        lon (rangef (- ^double pi) pi step-width)]
    (for [lat (rangef half-pi (- ^double half-pi) (- step-height))]
      (list lat lon))))

(defn combine-matrix
  "Add matrices `a` and `b`. A matrix is entered as a two-dimensional nested
   list; the number of rows and columns in `a` and `b` must be equal.

   Returns the sum of the two matrices, as a two-dimensional nested list."
  [a b]
  (loop [n 0 output (list)]
    (if (= n (count a))
      output
      (let [a-line (nth a n)
            b-line (nth b n)]
        (recur (inc n) (conj output (map + a-line b-line)))))))

(defn coverage-matrix
  [method sat-location dimensions]
  (let [matrix (blank-matrix dimensions)
        in-view? (view-fn method sat-location)
        cov-fn #(map in-view? (repeat (first %)) (second %))]
    (map cov-fn matrix)))

(defn coverage-combined
  [method sat-locations dimensions]
  (let [cov-fn #(coverage-matrix method % dimensions)]
    (reduce combine-matrix (map cov-fn sat-locations))))
