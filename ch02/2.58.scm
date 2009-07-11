(load "~/projects/sicp/ch02/deriv.scm")

;;a
(define (make-sum a1 a2)
  (cond ((and (number? a1) (number? a2)) (+ a1 a2))
        ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((and (number? m1) (number? m2)) (* m1 m2))
        ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s)
  (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

;;b

(define (sum? x)
  (and (pair? x) (>= (length x) 3) (eq? (cadr x) '+)))

(define (augend s)
  (let ((rest (cddr s)))
    (if (= (length rest) 1)
        (car rest)
        rest)))

(define (product? x)
  (and (pair? x) (>= (length x) 3) (eq? (cadr x) '*)))

(define (multiplicand p)
  (define (iter mul-expr expr)
    (if (or (null? expr) (eq? (car expr) '+))
        mul-expr
        (iter (append mul-expr (list (car expr))) (cdr expr))))
  (let ((result (iter '() (cddr p))))
    (if (= (length result) 1)
        (car result)
        result)))

