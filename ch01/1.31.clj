;;iter
(defn product [fn a next-fn b]
  (loop [result 1, i a]
    (if (> i b)
      result
      (recur (* result (fn i)) (next-fn i)))))

(defn factorial [n]
  (product identity 1 inc n))

;;recur
(defn product-r [fn a next-fn b]
  (if (> a b)
    1
    (* a (product-r fn (next-fn a) next-fn b))))


(defn pi [n]
  (defn next-fn [x] (+ x 2))
  (defn func [i] (/ (* (dec i) (inc i)) (float (* i i))))
  (* 4 (product func 3 next-fn n)))