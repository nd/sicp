(define (has-cycle? x)
  (define counted-pairs '())

  (define (already-counted? p)
    (= (length (filter (lambda (x) (eq? x p)) counted-pairs)) 1))

  (define (check l)
    (if (pair? l)
        (begin
          (set! counted-pairs (cons l counted-pairs))
          (if (already-counted? (cdr l))
              #t
              (check (cdr l))))
        #f))

  (check x))