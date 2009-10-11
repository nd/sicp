;; I think this is not a good thing to delete bindings from frames
;; other than first, because other procedures can become broken, so I
;; will delete binding only from first frame of environment which is
;; created when procedure is applied and contains only bindings for
;; procedure's parameters or bindings defined inside this procedure

(define (make-unbound! env var)
  (let ((frame (first-frame env)))
    (scan (frame-bindings frame)
          var
          (lambda ()) ;;do nothing if there is no binding
          (lambda (binding)
            (set-frame-bindings! frame
                                 (filter (lambda (b) (not (eq? (binding-var b) var)))
                                         (frame-bindings frame)))))))

