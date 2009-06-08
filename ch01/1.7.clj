;; ex 1.7

(defn good-enough? [prev-guess cur-guess]
  (< (/ (Math/abs (- cur-guess prev-guess)) 
        cur-guess)
     0.0001))