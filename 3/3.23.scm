;; double-linked list element structure
;; (stolen from eli bendersky)
;;
;;          +-------+ 
;;          | o | o | 
;;          +-|---|-+ 
;;            |   |
;;            |   V   
;;            |  next
;;            V
;;        +-------+  
;;        | o | o | 
;;        +-|---|-+ 
;;          V   |
;;        data  V
;;             prev
;;

(define (make-dl-item item)
  (cons (cons item '()) '()))
(define (set-next! item next-item)
  (if (not (eq? next-item '()))
      (let ((t (car next-item)))
        (set-cdr! t item)))
  (if (not (eq? item '()))
      (set-cdr! item next-item)))
(define (set-prev! item prev-item)
  (let ((t (car item)))
    (set-cdr! t prev-item))
  (if (not (eq? prev-item '()))
      (set-cdr! prev-item item)))
(define (next item)
  (cdr item))
(define (prev item)
  (cdar item))
(define (get-value item)
  (caar item))
(define (has-next? item)
  (eq? (cdr item) '()))
(define (empty? item)
  (or (eq? item '())
      (eq? (car item) '())))
(define (get-averse-list item)
  (get-list item next))
(define (get-reverse-list item)
  (get-list item prev))
(define (get-list item next-function)
  (if (empty? item)
      '()
      (cons (get-value item) (get-list (next-function item) next-function))))

;; dl tests:
(define dl1 (make-dl-item 'a))
(if (not (eq? (get-value dl1) 'a))
    (error "get-value doesn't work")
    "ok")

(if (empty? dl1)
    (error "empty? doesn't work")
    "ok")
(define dl2 (make-dl-item 'b))
(set-next! dl1 dl2)

(if (not (equal? (get-averse-list dl1) (list 'a 'b)))
    (error "get-averse-list doesn't work")
    "ok")

(if (not (equal? (get-reverse-list dl2) (list 'b 'a)))
    (error "set-next! doesn't work")
    "ok")

(define dl0 (make-dl-item 'c))
(set-prev! dl1 dl0)

(if (not (equal? (get-averse-list dl0) (list 'c 'a 'b)))
    (error "set-prev! doesn't work")
    "ok")

(if (not (equal? (get-reverse-list dl2) (list 'b 'a 'c)))
    (error "set-prev! doesn't work")
    "ok")

;;constructor
(define (make-deque)
  (cons '() '()))

;;predicat
(define (empty-deque? deque)
  (empty? (car deque)))

;;selectors
(define (front-deque deque)
  (if (empty-deque? deque)
      '()
      (get-averse-list (car deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      '()
      (get-reverse-list (cdr deque))))

;;mutators
(define (front-insert-deque! deque item)
  (let ((new-item (make-dl-item item)))
    (set-next! new-item (car deque))
    (if (empty-deque? deque)
        (set-cdr! deque new-item))
    (set-car! deque new-item)))

(define (rear-insert-deque! deque item)
  (let ((new-item (make-dl-item item)))
    (set-prev! new-item (cdr deque))
    (if (empty-deque? deque)
        (set-car! deque new-item))
    (set-cdr! deque new-item)))

(define (front-delete-deque! deque)
  (if (empty-deque? deque)
      (display "already empty")
      (let ((new-front (next (car deque))))
        (set-car! deque new-front)
        (set-prev! new-front '()))))

(define (rear-delete-deque! deque)
  (if (empty-deque? deque)
      (display "already empty")
      (let ((new-rear (prev (cdr deque))))
        (set-cdr! deque new-rear)
        (set-next! new-rear '()))))

;;tests
(define dq (make-deque))
(if (not (empty-deque? dq))
    (error "empty deque is not empty-deque?")
    "ok")

(equal? (front-deque dq) '())
(equal? (rear-deque dq) '())

(front-insert-deque! dq 'a)
(not (empty-deque? dq))
(equal? (front-deque dq) '(a))
(equal? (rear-deque dq) '(a))

(front-insert-deque! dq 'b)
(not (empty-deque? dq))
(equal? (front-deque dq) '(b a))
(equal? (rear-deque dq) '(a b))

(rear-insert-deque! dq 'c)
(not (empty-deque? dq))
(equal? (front-deque dq) '(b a c))
(equal? (rear-deque dq) '(c a b))

(front-delete-deque! dq)
(equal? (front-deque dq) '(a c))
(equal? (rear-deque dq) '(c a))

(rear-delete-deque! dq)
(equal? (front-deque dq) '(a))
(equal? (rear-deque dq) '(a))


(rear-insert-deque! dq 'c)
(equal? (front-deque dq) '(a c))
(equal? (rear-deque dq) '(c a))