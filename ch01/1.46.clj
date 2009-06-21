(defn iterative-improve [good-enough? improve]
  (fn [start-value]
    (loop [current-value start-value, prev-value (- start-value)]
      (if (good-enough? current-value prev-value)
        current-value
        (recur (improve current-value) current-value)))))


(defn sqrt [x]
  ((iterative-improve
    (fn [guess _] (< (Math/abs (- (* guess guess) x)) 0.001))
    (fn [guess] (/ (+ guess (/ x guess)) 2)))
   1.0))

(def tolerance 0.00001)

(defn fixed-point [f first-guess]
  (defn close-enough? [v1 v2]
    (< (Math/abs (- v1 v2)) tolerance))
  ((iterative-improve close-enough? f) first-guess))

