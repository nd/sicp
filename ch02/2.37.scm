(load "~/projects/sicp/ch02/accumulate.scm")
(load "~/projects/sicp/ch02/2.36.scm")

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map * (accumulate-n + 0 m) v))

(define (transponse m)
  (accumulate-n cons (list) m))

(define (matrix-*-matrix m n)
  (let ((cols (transponse n)))
    (map (lambda (row_j)
           (map (lambda (col_i) (dot-product row_j  col_i))
                cols))
         m)))
