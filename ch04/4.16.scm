;;a
(define (lookup-variable-value var env)
  (env-loop env
            (lambda () (error "Unbounded var" var))
            (lambda (frame) (scan (frame-bindings frame)
                             var
                             (lambda () (env-loop (enclosing-environment env)))
                             (lambda (binding)
                               (let ((value (binding-val bindings)))
                                 (if (eq? value '*unassigned*)
                                     (error "Unassigned variable" (binding-var bindings))
                                     value)))))))

;;b
(define (scan-out-defines body)
  (let* ((defs (filter definition? body))
         (non-defs (filter (lambda (x) (not (definition? x))) body))
         (assignments (map (lambda (def) (make-let-assignment (definition-variable def) ''*unassigned*)) defs))
         (sets (map (lambda (def) (make-assignment (definition-variable def) (definition-value def))) defs)))
    (if (not (null? defs))
        (list (let->combination (make-let assignments (append sets non-defs))))
        body)))

;;c
;see interpretator.scm