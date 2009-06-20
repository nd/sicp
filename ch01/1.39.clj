(load-file "1.37.clj")

(defn tan-cf [x k]
  (loop [i k, result 0]
    (if (= i 1)
      (/ x (- (* 2 i) 1 result))
      (recur (dec i)
             (/ (* x x) (- (* 2 i) 1 result))))))