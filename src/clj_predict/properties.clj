(ns clj-predict.properties
  "Physical properties and units used in calculation.")

(def celestial-default (atom :earth))

(defn celestial-default!
  [k]
  (reset! celestial-default k))

(defn celestial-body-factory
  [semi-major-axis flattening]
  (let [a semi-major-axis
        f flattening
        b (* a (- 1 f))
        r (-> (+ b (* 2 a)) (/ 3))
        e (- (* 2 f) (* f f))]
    {:semi-major-axis a :semi-minor-axis b :mean-radius r
     :coeff-flat f :ecc-squared e}))

(def celestial-bodies
  (atom {:earth   (celestial-body-factory   6378137 0.0033528)
         :moon    (celestial-body-factory   1738130   0.00125)
         :sun     (celestial-body-factory 696342000  0.000009)
         :mercury (celestial-body-factory   2439700         0)
         :venus   (celestial-body-factory   6051800         0)
         :mars    (celestial-body-factory   3396200   0.00589)
         :jupiter (celestial-body-factory  71492000   0.06487)
         :saturn  (celestial-body-factory  60268000   0.09796)
         :uranus  (celestial-body-factory  25559000    0.0229)
         :neptune (celestial-body-factory  24764000    0.0171)}))

(defn celestial-bodies!
  ([name]
    (swap! celestial-bodies dissoc name))
  ([name semi-major-axis flattening]
    (swap! celestial-bodies merge
           {name (celestial-body-factory semi-major-axis flattening)})))

(defn celestial-body
  ([]
    @celestial-default)
  ([name]
    (get @celestial-bodies name)))
