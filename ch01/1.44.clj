(def dx 0.0001)

(defn smooth [f]
  (fn [x] (/ (+ (f (- x dx)) (f x) (f (+ x dx))
             3))))

(defn smooth-n [f n]
  ((repeated smooth n) f))

