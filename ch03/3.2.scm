(define (make-monitored fun)
  (let ((counter 0))
    
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) counter)
            ((eq? m 'reset-count)
             (set! counter 0)
             'ok)
            (else
             (set! counter (+ counter 1))
             (fun m))))

    dispatch))