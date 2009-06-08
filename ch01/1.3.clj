;; ex. 1.3

(defn sum-of-squares
  "Sum of squares of two larger it's arguments"
  [a b c]
  (cond (and (< a b) (< a c)) (+ (* b b) (* c c))
        (and (< b a) (< b c)) (+ (* a a) (* c c))
        :else (+ (* a a) (* b b))))