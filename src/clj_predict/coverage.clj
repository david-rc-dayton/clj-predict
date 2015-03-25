(ns clj-predict.coverage)

(defn combine-matrix
  "Add matrices `a` and `b`. Matrices are entered as a two-dimensional list of
   lists of equal size, e.g:

   > `[[1 0 0] [0 1 0] [0 0 1]]`
   > `[[1 2 3] [4 5 6] [7 8 9]]`

   Returns the sum of the two matrices, as a list of lists."
  [a b]
  (when (= (count a) (count b))
    (loop [n 0 output []]
      (if (= n (count a))
        output
        (let [a-line (nth a n)
              b-line (nth b n)]
          (recur (inc n) (conj output (vec (map + a-line b-line)))))))))
