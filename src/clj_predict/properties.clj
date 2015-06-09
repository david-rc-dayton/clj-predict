(ns clj-predict.properties
  "Physical properties and units used in calculation.")

(def celestial-default (atom :earth))

(defn celestial-body
  []
  @celestial-default)

(defn celestial-default!
  [k]
  (reset! celestial-default k))

(def grav 6.67259e-20)

(defn celestial-body-factory
  [semi-major-axis flattening mass]
  (let [a semi-major-axis
        f flattening
        b (* a (- 1 f))
        r (-> (+ b (* 2 a)) (/ 3))
        e (- (* 2 f) (* f f))
        m mass
        mu (* grav mass)]
    {:semi-major-axis a :semi-minor-axis b :mean-radius r
     :coeff-flat f :ecc-squared e :m m :mu mu}))

(def celestial-bodies
  (atom {:earth   (celestial-body-factory 6378.137 0.0033528  5.9737e24)
         :moon    (celestial-body-factory  1738.14   0.00125  7.3477e22)
         :sun     (celestial-body-factory   696342  0.000009  1.9885e30)
         :mercury (celestial-body-factory   2439.7         0  3.3022e23)
         :venus   (celestial-body-factory   6051.8         0  4.8676e24)
         :mars    (celestial-body-factory   3396.2   0.00589  6.4185e23)
         :jupiter (celestial-body-factory    71492   0.06487  1.8986e27)
         :saturn  (celestial-body-factory    60268   0.09796  5.6846e26)
         :uranus  (celestial-body-factory    25559    0.0229  8.6810e25)
         :neptune (celestial-body-factory    24764    0.0171  1.0243e26)}))

(defn celestial-map
  [k]
  (get @celestial-bodies k))

(defn celestial-bodies!
  ([name]
    (swap! celestial-bodies dissoc name))
  ([name semi-major-axis flattening mass]
    (swap! celestial-bodies merge
           {name (celestial-body-factory semi-major-axis flattening mass)})))
