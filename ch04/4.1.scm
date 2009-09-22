;; from left to right
(define (list-of-values exps env)
  (define (iter exps result)
    (if (no-operands? exps)
        result
        (iter (rest-operands exps)
              (append result (list (eval (first-operand exps) env))))))
  (iter exps '()))

;; from right to left
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-values (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              rest-values))))