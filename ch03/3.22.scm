(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr  '()))

    (define (empty-queue?) (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
          (error "front-queue called on empty queue")
          (car front-ptr)))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr  new-pair)
               dispatch)
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               dispatch))))

    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "delete-queue! called on empty queue"))
            (else
             (set! front-ptr (cdr front-ptr))
             dispatch)))

    (define (print)
      front-ptr)

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print) print)
            (else (error "Unknown message " m))))

    dispatch))

(define (empty-queue? queue)
  ((queue 'empty-queue?)))

(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))

(define (print-queue queue)
  ((queue 'print)))

(define (delete-queue! queue)
  ((queue 'delete-queue!)))

(define (front-queue queue)
  ((queue 'front-queue)))