(define (has-cycle? x)
  (define current-number 0)

  (define visited-value 'no-value)
  (define visited-number 0)

  (define (remember-pair p)
    (set! visited-value p)
    (set! visited-number current-number))

  (define (check l)
    (set! current-number (+ current-number 1))

    (if (> current-number (* visited-number 2))
        (remember-pair l))

    (if (eq? l visited-value)
        #t
        (check (cdr l))))

  (check x))