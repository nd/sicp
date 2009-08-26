; deque structure:
; forward-front-ptr                     forward-rear-ptr   
;        |                                      |
;        v                                      v
;    +---+---+    +---+---+    +---+---+    +---+---+
;    | o | o-+--->| o | o-+--->| o | o-+--->| o |nil|
;    +-+-+-^-+    +-+-+-^-+    +-+-+-^-+    +-+-+-^-+
;      |   |        |   |        |   |        |   |
;      |   |        |   |        |   |        |   |
;    +-v-+ |      +-v-+ |      +-v-+ |      +-v-+ |
;    | a | |      | b | |      | c | |      | d | |
;    +---+ |      +---+ |      +---+ |      +---+ |
;          |            |            |            |
;    +---+-+-+    +---+-+-+    +---+-+-+    +---+-+-+
;    |nil| o +<---+-o | o +<---+-o | o |<---+-o | o +
;    +---+---+    +-+-+---+    +-+-+---+    +---+---+
;        ^                                      ^
;        |                                      |
; backward-rear-ptr                    backward-front-ptr


(define (make-deque) (list '()   ;<-forward-front-ptr
                           '()   ;<-forward-rear-ptr
                           '()   ;<-backward-front-ptr
                           '())) ;<-backward-rear-ptr

(define (forward-front-ptr deque) (car deque))
(define (forward-rear-ptr deque) (cadr deque))
(define (backward-front-ptr deque) (caddr deque))
(define (backward-rear-ptr deque) (cadddr deque))

(define (set-forward-front-ptr! deque item) (set-car! deque item))
(define (set-forward-rear-ptr! deque item) (set-car! (cdr deque) item))
(define (set-backward-front-ptr! deque item) (set-car! (cddr deque) item))
(define (set-backward-rear-ptr! deque item) (set-car! (cdddr deque) item))

(define (print-deque deque) (forward-front-ptr deque))

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      '()
      (car (forward-front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      '()
      (car (backward-front-ptr deque))))

(define (front-insert-deque! deque item)
  (cond ((empty-deque? deque)
         (let* ((new-forward-pair (cons item '()))
                (new-backward-pair (cons new-forward-pair '())))
           (set-forward-front-ptr!  deque new-forward-pair) 
           (set-forward-rear-ptr!   deque new-forward-pair) 
           (set-backward-front-ptr! deque new-backward-pair) 
           (set-backward-rear-ptr!  deque new-backward-pair) 
           deque))
        (else
         (let* ((new-forward-pair (cons item (forward-front-ptr deque)))
                (new-backward-pair (cons new-forward-pair '())))
           (set-forward-front-ptr! deque new-forward-pair)
           (set-cdr! (backward-rear-ptr deque) new-backward-pair)
           (set-backward-rear-ptr! deque new-backward-pair)
           deque))))

(define (rear-insert-deque! deque item)
  (cond ((empty-deque? deque)
         (let* ((new-forward-pair (cons item '()))
               (new-backward-pair (cons new-forward-pair '())))
           (set-forward-front-ptr!  deque new-forward-pair) 
           (set-forward-rear-ptr!   deque new-forward-pair) 
           (set-backward-front-ptr! deque new-backward-pair) 
           (set-backward-rear-ptr!  deque new-backward-pair) 
           deque))
        (else
         (let* ((new-forward-pair (cons item '()))
                (new-backward-pair (cons new-forward-pair (backward-front-ptr deque))))
           (set-cdr! (forward-rear-ptr deque) new-forward-pair)
           (set-forward-rear-ptr!   deque new-forward-pair)
           (set-backward-front-ptr! deque new-backward-pair)
           deque))))

(define (front-delete-deque! deque)
  (if (empty-deque? deque)
      (error "deque is empty")
      (let ((forward-second (cdr (forward-front-ptr deque))))
        (set-forward-front-ptr! deque forward-second)
        deque)))

(define (rear-delete-deque! deque)
  (if (empty-deque? deque)
      (error "deque is empty")
      (let* ((backward-second (cdr (backward-front-ptr deque))))
        (cond ((= (length (forward-front-ptr deque)) 1)
               (set-forward-front-ptr!  deque '()) 
               (set-forward-rear-ptr!   deque '()) 
               (set-backward-front-ptr! deque '()) 
               (set-backward-rear-ptr!  deque '()) 
               deque)
              (else
               (let ((forward-rear-second (car backward-second)))
                 (set-cdr! forward-rear-second '())
                 (set-forward-rear-ptr! deque forward-rear-second)
                 (set-backward-front-ptr! deque backward-second)
                 deque))))))
