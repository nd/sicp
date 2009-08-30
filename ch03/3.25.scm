(define (make-table . same-key?)
  (let ((local-table (list '*table*))
        (equal-p (if (null? same-key?) eq? same-key?)))

    (define (assoc key records)
      (cond ((null? records) false)
            ((equal-p key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (lookup keys)
      (define (iter keys table)
        (if (null? table)
            false
            (if (null? keys)
                table
                (if (list? table)
                    (let ((subtable (assoc (car keys) table)))
                      (if subtable
                          (iter (cdr keys) (cdr subtable))
                          false))
                    false))))
      (iter keys (cdr local-table)))

    (define (insert! keys value)
      (define (iter keys table)
        (if (null? keys)
            (set-cdr! table value)
            (if (list? (cdr table))
                (let ((subtable (assoc (car keys) (cdr table))))
                  (if subtable
                      (iter (cdr keys) subtable)
                      (let ((new-subtable (cons (car keys) '())))
                        (set-cdr! table (cons new-subtable (cdr table)))
                        (iter (cdr keys) new-subtable))))
                (let ((new-subtable (cons (car keys) '())))
                  (set-cdr! table (cons new-subtable '()))
                  (iter (cdr keys) new-subtable)))))
      (iter keys local-table)
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'insert!) insert!)
            ((eq? m 'lookup) lookup)
            ((eq? m 'local-table) (lambda () local-table))
            (else "unknown message " m)))

    dispatch))