(defn cubic [a b c]
  (fn [x] (+ (* x x x) (* a x x) (* b x) c)))