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
                               (merge-weighted (stream-cdr s2)
                                               (stream-cdr s1)
                                                weight))))))))

(define (weighted-pairs s t weight)
       (cons-stream
        (list (stream-car s) (stream-car t))
        (merge-weighted (stream-map (lambda (x) (list (stream-car s) x))
                                    (stream-cdr t))
                        (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
                        weight)))

;;a
(display-stream (weighted-pairs integers
                                integers
                                (lambda (x) (+ (car x) (cadr x)))))
;;b
(display-stream
 (stream-filter (lambda (pair)
                  (let ((i (car pair))
                        (j (cadr pair)))
                    (not (or
                          (= (remainder i 2) 0)
                          (= (remainder i 3) 0)
                          (= (remainder i 5) 0)
                          (= (remainder j 2) 0)
                          (= (remainder j 3) 0)
                          (= (remainder j 5) 0)))))
                (weighted-pairs integers
                                integers
                                (lambda (x)
                                  (let ((i (car x))
                                        (j (cadr x)))
                                    (+ (* 2 i) (* 3 j) (* 5 i j)))))))