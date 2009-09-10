(define (weight x)
  (let ((i (car x))
        (j (cadr x)))
    (+ (* i i) (* j j))))

(define p (weighted-pairs integers
                          integers
                          weight))

(define squares
  (stream-filter (lambda (x) (> x 0))
                 (stream-map
                  (lambda (x y z)
                    (if (= (weight x) (weight y) (weight z))
                        (begin
                          (display x)
                          (display y)
                          (display z)
                          (newline)
                          (weight x))
                        0))
                  p
                  (stream-cdr p)
                  (stream-cdr (stream-cdr p)))))