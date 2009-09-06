(define (div-streams s1 s2)
  (stream-map (lambda (x y) (/ x y))
              s1 s2))

(define (integrate-series s)
  (mul-streams (div-streams ones integers) s))

(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))