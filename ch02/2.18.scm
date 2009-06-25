(define (reverse l)
  (if (null? (cdr l))
      (list (car l))
      (append (reverse (cdr l)) (list (car l)))))