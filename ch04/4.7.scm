(define (let*->nested-lets exp)
  (define (iter assignments)
    (if (null? (cdr assignments))
        (make-let (car assignments) (sequence->exp (let-body exp)))
        (make-let (car assignments) (iter (cdr assignments)))))
  (iter (let-assignments exp)))

(define (make-let assignments body) (list 'let assignments body))
