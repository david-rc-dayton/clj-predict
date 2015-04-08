(ns clj-predict.coverage
  "Matrix generation functions for satellite coverage analysis.")

(def pi (Math/PI))
(def two-pi (* 2 pi))
(def half-pi (/ pi 2))

(def ascii-legend
  {0 ". " 1 "- " 2 "= "
   3 "% " 4 "+ " 5 "V "
   :default "@ "})

(defn print-matrix
  [m]
  (let [replace-fn #(or (get ascii-legend %) (:default ascii-legend))
        str-fn #(apply str (map replace-fn %))]
    (dorun (map #(println (str-fn %)) m))))

(defn rangef
  [start end step]
  (map #(+ start (* % step))
       (range 0 (/ (Math/abs ^double (- end start))
                   (Math/abs ^double step)) 1)))

(defn cov-cosine
  [phi-1 lam-1 phi-2 lam-2]
  (let [delta-phi-two (-> (Math/sin (/ (- phi-2 phi-1) 2)) (Math/pow 2))
        delta-lam-two (-> (Math/sin (/ (- lam-2 lam-1) 2)) (Math/pow 2))
        a (+ delta-phi-two (* (Math/cos phi-1) (Math/cos phi-2) delta-lam-two))]
    (* 2 (Math/atan2 (Math/sqrt a) (Math/sqrt (- 1 a))))))

(def cov-methods
  {:cosine cov-cosine})

(defn view-fn
  [method {:keys [lat lon alt]}]
  (fn [gnd-lat gnd-lon]
    (let [sat-lat (* lat (/ pi 180))
          sat-lon (* lon (/ pi 180))
          view-limit (Math/acos (/ 6371000 (+ 6371000 alt)))
          view-method (get cov-methods method)]
      (if (<= (view-method sat-lat sat-lon gnd-lat gnd-lon) view-limit)
        1 0))))

(defn blank-matrix
  "Generate a blank coverage matrix. Takes the argument:

   > `dimensions` - a vector of the `width` & `height` for the desired matrix

   Returns a matrix containing coordinates of the form
   `[latitude [longitudes]]`, in radians."
  [[width height :as dimensions]]
  (let [step-width (/ two-pi width)
        step-height (- (/ pi height))
        lon (rangef (- pi) pi step-width)]
    (for [lat (rangef half-pi (- half-pi) step-height)] [lat lon])))

(defn combine-matrix
  "Add matrices `a` and `b`. A matrix is entered as a two-dimensional nested
   list; the number of rows and columns in `a` and `b` must be equal.

   Returns the sum of the two matrices, as a two-dimensional nested list."
  [a b]
  (loop [n 0 output []]
    (if (= n (count a))
      output
      (let [a-line (nth a n)
            b-line (nth b n)]
        (recur (inc n) (conj output (map + a-line b-line)))))))

(defn coverage-matrix
  [method {:keys [lat lon alt] :as satellite} [width height :as dimensions]]
  (let [matrix (blank-matrix dimensions)
        in-view? (view-fn method satellite)]
    (loop [m matrix o []]
      (if (empty? m)
        o
        (let [row  (first m)
              phi  (first row)
              lams (second row)
              rep  (map (partial in-view? phi) lams)]
          (recur (rest m) (conj o rep)))))))

(defn coverage-combined
  [method locations [width height :as dimensions]]
  (let [cov-fn #(coverage-matrix method % dimensions)]
    (reduce combine-matrix (map cov-fn locations))))

(defn coverage-indexed
  [method locations [width height :as dimensions]]
  (let [cov-fn #(coverage-matrix method % dimensions)]
    (zipmap (range) (map cov-fn locations))))
