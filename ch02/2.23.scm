(define (for-each proc items)
  (cond ((null? items) true)
        (else (proc (car items))
              (for-each proc (cdr items)))))