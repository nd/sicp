(load-file "gcd.clj")

(defn make-rat [n d] 
  (let [g (gcd (Math/abs n) (Math/abs d))]
    (cons (* (if (> (* n d) 0) 1 (- 1)) 
             (/ (Math/abs n) g))
          [(/ (Math/abs d) g)])))

(defn numer [x] (first x))

(defn denom [x] (second x))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

