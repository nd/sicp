(define (square-list items)
  (define (iter things result)
    (if (null? things)
        result
        (iter (cdr things)
              (append result (list (square (car things)))))))
  (iter items (list)))