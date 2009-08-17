(define rand
  (let ((current-value random-init))
    (lambda (m)
      (cond ((eq? m 'generate)
           (begin
             (set! current-value (rand-update current-value))
             current-value))
          ((eq? m 'reset)
           (lambda (new-value)
             (set! current-value new-value)
             current-value))
          (else (error "Unknown message " m))))))

(define (rand-update x) (+ x 1))

(define random-init 0)