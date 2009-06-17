(define f
  (let ((state 'undefined))
    (lambda (x)
      (if (eq? state 'undefined) (set! state x))
      state)))