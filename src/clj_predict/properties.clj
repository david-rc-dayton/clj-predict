(ns clj-predict.properties
  "Physical properties and units used in calculation.")

(def grav 6.67259e-20)

(defn body-factory
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

(def body-properties
  (atom {:earth   (body-factory
                    6378.137 0.0033528 5.9737e24)
         :moon    (body-factory
                    1738.14 0.00125 7.3477e22)
         :sun     (body-factory
                    696342 0.000009 1.9885e30)
         :mercury (body-factory
                    2439.7 0 3.3022e23)
         :venus   (body-factory
                    6051.8 0 4.8676e24)
         :mars    (body-factory
                    3396.2 0.00589 6.4185e23)
         :jupiter (body-factory
                    71492 0.06487 1.8986e27)
         :saturn  (body-factory
                    60268 0.09796 5.6846e26)
         :uranus  (body-factory
                    25559 0.0229 8.6810e25)
         :neptune (body-factory
                    24764 0.0171 1.0243e26)}))

(defn body
  [k]
  (get @body-properties k))

(defn body-properties!
  ([name]
    (swap! body-properties dissoc name))
  ([name semi-major-axis flattening mass]
    (swap! body-properties merge
           {name (body-factory semi-major-axis flattening mass)})))

(defn orbit-factory
  [semi-major-axis eccentricity period rotation axial-tilt]
  {:semi-major-axis semi-major-axis :ecc eccentricity
   :period period :rotation rotation :tilt axial-tilt})

(def orbit-properties
  (atom {:earth   (orbit-factory
                    149598261 0.01671123 365.256363004 0.99726968 23.439281)
         :moon    (orbit-factory
                    384399 0.0549 27.321582 27.321582 6.687)
         :mercury (orbit-factory
                    57909050 0.205630 87.9691 58.646 2.04)
         :venus   (orbit-factory
                    108208000 0.0067 224.701 -243.0185 177.36)
         :mars    (orbit-factory
                    227939100 0.0935 686.971 1.025957 25.19)
         :jupiter (orbit-factory
                    778547200 0.048775 4332.59 0.4135 3.13)
         :saturn  (orbit-factory
                    1433449370 0.055723219 10759.22 0.4396 26.73)
         :uranus  (orbit-factory
                    2870671400 0.047220087 30687.15 -0.71833 97.77)
         :neptune (orbit-factory
                    4498542600 0.00867797 60190.03 0.6713 28.32)}))

(defn orbit
  [k]
  (get @orbit-properties k))

(defn orbit-properties!
  ([name]
    (swap! orbit-properties dissoc name))
  ([name semi-major-axis eccentricity period rotation axial-tilt]
    (swap! orbit-properties merge
           {name (orbit-factory semi-major-axis eccentricity 
                                period rotation axial-tilt)})))
