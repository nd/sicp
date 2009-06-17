(defn sum [term a next-f b]
  (loop [current-sum 0, i a]
    (if (> i b)
      current-sum
      (recur (+ current-sum (term i)) (next-f i)))))

(defn integral [f a b dx]
  (defn add-dx [x] (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

; h                                                                     
; - (y_0 + 4y_1 + 2y_2 + 4y_3 + 2y_4 + ... + 2y_(n-2) + 4y_(n-1) + y_n) 
; 3                                                                     
(defn integral-sympson [f a b n]
  (defn h [] (/ (- b a) (float n)))
  (defn fk [k] (f (+ a (* k (h)))))
  (defn koef [k]
    (cond (or (= k 0) (= k n)) 1
          (even? k) 2
          :else 4))
  (defn term [i] (* (koef i) (fk i)))
  (/ (* (sum termf 0 inc n)
        (h))
     3.0))