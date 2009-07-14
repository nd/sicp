(define (union-set s1 s2)
  (define (iter union set-to-add)
    (if (null? set-to-add)
        union
        (if (element-of-set? (car set-to-add) union)
            (iter union (cdr set-to-add))
            (iter (adjoin-set (car set-to-add) union) (cdr set-to-add)))))
  (iter s2 s1))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


    