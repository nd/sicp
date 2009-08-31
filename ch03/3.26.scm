;; skip this ex.
(define (make-table)
  (let ((local-table (list '*table*))
        (> (lambda (a b) (string>? a b)))
        (< (lambda (a b) (string<? a b)))
        (= (lambda (a b) (string=? a b))))

    (define (make-record key value) (cons key value))
    (define (key   record) (car record))
    (define (value record) (cdr record))

    (define (make-tree entry left right) (list entry list right))
    (define (entry        tree) (car tree))
    (define (left-branch  tree) (cadr tree))
    (define (right-branch tree) (caddr tree))

    (define (assoc key tree)
      (cond ((null? tree) false)
            ((= key (key (entry tree))) (value (entry tree)))
            ((< key (key (entry tree))) (assoc (key (left-branch  tree))))
            ((> key (key (entry tree))) (assoc (key (right-branch  tree))))))

    (define (lookup keys)
      (define (iter keys table)
        (if (null? table)
            false
            (if (null? keys)
                table
                (let ((subtable (assoc (car keys) table)))
                  (if subtable
                      (iter (cdr keys) (cdr subtable))
                      false)))))
      (iter keys (cdr local-table)))

    ;;todo: rewrite to work with trees
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