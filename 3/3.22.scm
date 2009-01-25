(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr  '()))

    (define (empty?)
      (eq? front-ptr '()))

    (define (insert item)
      (if (empty?)
          (begin
            (set! front-ptr (cons item '()))
            (set! rear-ptr  front-ptr))
          (begin
            (set-cdr! rear-ptr (cons item '()))
            (set! rear-ptr (cdr rear-ptr)))))
      

    (define (delete)
      (if (empty?)
          (error "Queue is empty")
          (set! front-ptr (cdr front-ptr))))

    (define (print)
      (display front-ptr))

    (define (dispatch m)
      (cond ((eq? m 'insert) insert)
            ((eq? m 'delete) delete)
            ((eq? m 'print)  print)
            (else (error "Unknown operation" m))))
    dispatch))