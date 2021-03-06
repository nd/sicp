(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs (map (lambda (e)
                       (if (label-exp? e)
                           (error "Label as argument to operation -- make-operation-exp" e)
                           (make-primitive-exp e machine labels)))
                     (operation-exp-operands exp))))
    (lambda () (apply op (map (lambda (p) (p)) aprocs)))))