(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coef higher-terms) (+ this-coef
                                             (* x higher-terms)))
              0
              coefficient-sequence))