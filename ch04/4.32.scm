;; lazy arguments allow us to define solve and solve-2nd like this:

(define (solve f y0 dt)
  (define y (integral (map f y) y0 dt))
  y)

(define (solve-2nd a b dt y0 dy0)
  (define y (integral dy y0 dt))
  (define dy (integral (add-lists (scale-list y b) (scale-list dy a)) dy0 dt))
  y)