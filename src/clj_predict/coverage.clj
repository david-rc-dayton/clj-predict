(ns clj-predict.coverage
  "Matrix generation functions for satellite coverage analysis.")

(defn coordinate-matrix
  "Generate a blank coverage matrix. Takes the argument:

   > `dimensions` - a vector of the `width` & `height` for the desired matrix

   Returns a matrix containing coordinates of the form `[latitude longitude]`,
   in degrees."
  [[width height :as dimensions]]
  (let [step-width (double (/ 360 width))
        step-height (- (double (/ 180 height)))]
    (partition width (for [lat (range 90.0 -90.0 step-height)
                           lon (range -180.0 180.0 step-width)]
                       [lat lon]))))

(defn combine-matrix
  "Add matrices `a` and `b`. A matrix is entered as a two-dimensional nested
   vector; the number of rows and columns in `a` and `b` must be equal.

   Returns the sum of the two matrices, as a two-dimensional nested vector."
  [a b]
  (loop [n 0 output []]
    (if (= n (count a))
      output
      (let [a-line (nth a n)
            b-line (nth b n)]
        (recur (inc n) (conj output (vec (map + a-line b-line))))))))
