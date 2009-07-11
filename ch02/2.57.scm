(load "~/projects/sicp/ch02/2.56.scm")

(define (has-two-args? expr)
  (= (length expr) 3))

(define (get-2nd-arg-or-rest expr operator)
  (if (has-two-args? expr)
      (caddr expr)
      (append (list operator) (cddr expr))))

(define (augend s)
  (get-2nd-arg-or-rest s '+))

(define (multiplicand p)
  (get-2nd-arg-or-rest p '*))