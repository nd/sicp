(define (count-pairs x)
  (define count-new-pairs
    (let ((old-pairs (list)))
      (lambda (x)
        (if (not (pair? x))
            0
            (begin
              (let ((new (not (memq x old-pairs))))
                (set! old-pairs (cons x old-pairs))
                (+ (count-new-pairs (car x))
                   (count-new-pairs (cdr x))
                   (if new 1 0))))))))
  (count-new-pairs x))

;;tests:
(define three '(a b c))
(if (= (count-pairs three) 3)
    'ok
    (error "this structure has 3 pairs"))

(define d (list 'a))
(if (= (count-pairs (cons d (cons 'b d))) 3)
    'ok
    (error "this structure has 3 pairs"))

(define a (list 'a))
(define b (cons a a))
(define c (cons b b))
(if (= (count-pairs c) 3)
    'ok
    (error "this structure has 3 pairs"))



