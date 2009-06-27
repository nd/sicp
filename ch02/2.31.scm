(define (tree-map proc tree)
  (map (lambda (sub-tree) 
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (square sub-tree)))
       tree))



(define (square-tree tree)
  (tree-map square tree))