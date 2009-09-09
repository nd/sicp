(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (p) (list (stream-car s) (car p) (cadr p)))
                (pairs (stream-cdr t) (stream-cdr s)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define pithagor-stream
  (stream-filter (lambda (triple)
                   (let ((i (car triple))
                         (j (cadr triple))
                         (k (caddr triple)))
                     (eq? (+ (* i i) (* j j))
                          (* k k))))
                 (triples integers integers integers)))