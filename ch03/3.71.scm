(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car))
                  (cons-stream s1car (merge-weighted (stream-cdr s1)
                                                     s2
                                                     weight)))
                 ((> (weight s1car) (weight s2car))
                  (cons-stream s2car (merge-weighted s1
                                                     (stream-cdr s2)
                                                     weight)))
                 (else
                  (cons-stream s1car
                               (merge-weighted s2
                                               (stream-cdr s1)
                                                weight))))))))

(define (weight x)
  (let ((i (car x))
        (j (cadr x)))
    (+ (* i i i) (* j j j))))

(define p (weighted-pairs integers
                          integers
                          weight))

(define rumanujan-stream
  (stream-filter (lambda (x) (> x 0))
                 (stream-map
                  (lambda (x y)
                    (if (= (weight x) (weight y))
                        (weight x)
                        0))
                  (stream-cdr p)
                  p)))

;;1729 4104 13832 20683 32832 39312 