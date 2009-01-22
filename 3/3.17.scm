(define (count-pairs x)
  (define counted-pairs '())

  (define (already-counted? p)
    (= (length (filter (lambda (x) (eq? x p)) counted-pairs)) 1))

  (define (count s)
    (if (not (pair? s))
        0
        (+ (count (car s))
           (count (cdr s))
           (if (already-counted? s)
               0
               (begin
                 (set! counted-pairs (cons s counted-pairs))
                 1)))))
  
  (count x))

