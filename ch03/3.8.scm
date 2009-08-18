(define f
  (let ((state 'no-value))
    (lambda (x)
      (if (eq? state 'no-value)
          (set! state x))
      state)))

