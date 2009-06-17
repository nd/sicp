(define (make-accumulator initial)
  (lambda (x)
    (set! initial (+ initial x))
    initial))
