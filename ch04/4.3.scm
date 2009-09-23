(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else
         (let ((op (get 'eval (car exp))))
           (cons ((not (null? op)) (op exp env))
                 ((application? exp) (apply (eval (operator exp) env)
                                           (list-of-values (operands exp) env)))
                 (else (error "Unknown expression type -- EVAL" exp)))))))