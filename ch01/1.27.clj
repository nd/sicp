(defn square [x] (* x x))

(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp) (rem (square (expmod base (/ exp 2) m)) m)
        :else (rem (* base (expmod base (- exp 1) m)) m)))

(defn fool-fermat? [x]
  (defn iter [i]
    (if (not (= (expmod i x x) i))
      (= i x)
      (recur (inc i))))
  (iter 2))