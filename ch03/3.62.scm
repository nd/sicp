(define (div-series s1 s2)
  (if (null? (stream-car s2))
      (error "Division by zero")
      (mul-series s1 (scale-stream (invert-unit-series s2) (stream-car s2)))))