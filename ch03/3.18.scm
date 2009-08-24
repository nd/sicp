(define (has-cycle? items)
  (define has?
    (let ((pairs '()))
      (lambda (i)
        (if (not (pair? i))
            false
            (begin
              (if (memq i pairs)
                  true
                  (begin
                    (set! pairs (cons i pairs))
                    (has? (cdr i)))))))))
  (has? items))