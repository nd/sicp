;; ex: variable lookup in deeply nested environment
(define (f n)
  (let ((a n))
    (let ((b a))
      (let ((c b))
        (let ((d c))
          (let ((e d))
            (let ((f e))
              (define (iter i)
                (if (= i n)
                    'ok
                    (iter (+ i 1))))
              (iter 0))))))))