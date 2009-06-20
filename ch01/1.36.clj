(load-file "fixed-point.clj")

(fixed-point (fn [x] (/ (Math/log 1000) (Math/log x))) 2.0)