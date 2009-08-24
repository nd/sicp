(define (has-cycle? items)
  (define has?
    (let ((current-element '())
          (element-number 0))
      (lambda (i j)
        (if (not (pair? i))
            false
            (if (eq? i current-element)
                true
                (begin
                  (if (> j (* element-number 2))
                      (begin
                        (set! element-number j)
                        (set! current-element i)))
                  (has? (cdr i) (+ j 1))))))))
  (has? items 1))