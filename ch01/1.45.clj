(load-file "1.43.clj")
(load-file "fixed-point.clj")

(defn average-damp [f]
  (fn [x] (/ (+ x (f x)) 2)))

(defn power [x n]
  (if (= n 0)
    1
    ((repeated #(* % x) n) 1)))

(defn sqrt [x n]
  (fixed-point (average-damp (fn [y] (/ x (power y (dec n)))))
               1.0))