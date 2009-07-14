(define (union-set s1 s2)
  (define (iter union set-to-add)
    (if (null? set-to-add)
        union
        (iter (adjoin-set (car set-to-add) union) (cdr set-to-add))))
  (iter s2 s1))

;;no changes in element-of-set?
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

;;intersection-set - same as union-set