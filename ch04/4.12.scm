(define (scan bindings var null-bindings-proc var-found-proc)
  (cond ((null? bindings) (null-bindings-proc))
        ((eq? var (binding-var (car bindings))) (var-found-proc binding))
        (else (scan (cdr bindings) var null-bindings-proc var-found-proc))))

(define (env-loop env empty-env-proc frame-scan-proc)
  (if (eq? env the-empty-environment)
      (empty-env-proc)
      (let ((frame (first-frame env)))
        (frame-scan-proc))))

(define (lookup-variable-value var env)
  (env-loop env
            (lambda () (error "Unbounded var" var))
            (lambda (frame) (scan (frame-bindings frame)
                             var
                             (lambda () (env-loop (enclosing-environment env)))
                             (lambda (binding) (binding-val bindings))))))

(define (set-variable-value! var val env)
  (env-loop env
            (lambda () (error "Unknown variable" var))
            (lambda (frame) (scan (frame-bindings frame)
                             var
                             (lambda () (env-loop (enclosing-environment env)))
                             (lambda (binding) (set-binding-val! binding val))))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (scan (frame-bindings frame)
          var
          (lambda () (add-binding-to-frame! (make-binding var val) frame))
          (lambda (binding) (set-binding-val! binding val)))))

