(def tolerance 0.00001)

(defn fixed-point [f first-guess]
  (defn close-enough? [v1 v2]
    (< (Math/abs (- v1 v2)) tolerance))
  (defn try-guess [guess]
    (let [next (f guess)]
      (println next)
      (if (close-enough? guess next)
        next
        (try-guess next))))
  (try-guess first-guess))