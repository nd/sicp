;;iter
(defn cont-frac [n d k]
  (loop [i k, result 0]
    (if (= i 1)
      (/ (n i) 
         (+ (d i) result))
      (recur (dec i) 
             (/ (n i) 
                (+ (d i) result))))))

;;to get 4 right digits k should be = 11

(defn cont-frac-r [n d k]
  (defn compute [i]
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (compute (inc i))))))
  (compute 1))