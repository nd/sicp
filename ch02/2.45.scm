(define (split big-f small-f)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split big-f small-f) painter (- n 1))))
          (big-f painter (small-f smaller smaller))))))