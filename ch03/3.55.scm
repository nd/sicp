(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s)
                            (partial-sums s))))