(define (square-tree1 tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree1 (car tree))
                    (square-tree1 (cdr tree))))))

(define (square-tree2 tree)
  (map (lambda (x)
         (if (pair? x)
             (square-tree2 x)
             (square x)))
       tree))
        