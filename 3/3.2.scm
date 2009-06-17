(define (make-monitored f)
  (let ((calls 0))

    (define (call-and-count x)
      (set! calls (+ calls 1))
      (f x))

    (define (reset-count)
      (set! calls 0)
      "reset count of calls to zero")

    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) calls)
            ((eq? m 'reset-count) (reset-count))
            (else (call-and-count m))))

    dispatch))