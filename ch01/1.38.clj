(load-file "1.37.clj")

(cont-frac (fn [x] 1.0)
           (fn [x] 
             (cond (< x 3) x
                   (= (rem (- x 2) 3) 0) (+ 2 (* 2 (/ (- x 2) 3)))
                   :else 1))
           10)