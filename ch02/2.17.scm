(define (last-pair l)
  (if (null? (cdr l))
      (list (car l))
      (last-pair (cdr l))))