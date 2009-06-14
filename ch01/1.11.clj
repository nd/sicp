;; recursive process:
(defn f [n]
  (if (< n 3)
    n
    (+ (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

;; iterative process:
(defn fi [n]
  (defn iter [i fn-1 fn-2 fn-3]
    (if (= i n)
      (+ fn-1
         (* 2 fn-2)
         (* 3 fn-3))
      (recur (inc i) 
             (+ fn-1
                (* 2 fn-2)
                (* 3 fn-3))
             fn-1 
             fn-2)))
  (if (< n 3) 
      n
      (iter 3 2 1 0)))



