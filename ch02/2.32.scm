(define (subsets s)
  (if (null? s)
      (list (list))
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset) (cons (car s) subset))
                          rest)))))

;;subsets of empty set is ()
;;subsets of non-empty set is (subsets of set without first elem) with added first element 