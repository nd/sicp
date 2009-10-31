(define (an-integer-between x y)
  (require (<= x y))
  (amb x (an-integer-between (+ x 1) y)))