(ns clj-predict.coverage
  "Matrix generation functions for satellite coverage analysis.")

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
