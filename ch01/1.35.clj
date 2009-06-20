(load-file "fixed-point.clj")

(fixed-point (fn [x] (+ 1 (/ 1 x))) 1.0)

