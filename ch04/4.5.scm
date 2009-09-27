(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no `else' clause
      (let ((first (car clauses))
            (rest  (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-application
             (make-lambda '(x)
                          (make-if x
                                   (if (eq? (car (cond-actions first)) '=>)
                                       (make-application (cadr (cond-actions first)) x)
                                       (sequence->exp (cond-actions first)))
                                   (expand-clauses rest)))
             (cond-predicate first))))))

(define (make-application proc . args)
  (append (list proc) args))


;(cond (first-predicate => first-consumer)
;      (second-predicate => second-consumer)
;      (third-predicate third-actions)
;      (else else-actions))
;
;is the same as:
;
;((lambda (x)
;   (if x
;       (first-consumer x)
;       ((lambda (x)
;           (if x
;               (second-consumer x)
;               ((lambda (x)
;                  (if x
;                      (begin third-actions)
;                      (begin else-actions))) third-predicate))) second-predicate))) first-predicate)