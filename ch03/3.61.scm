(define (invert-unit-series s)
  (cons-stream 1 (scale-stream (mul-series s
                                           (invert-unit-series s))
                               -1)))