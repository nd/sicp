(define (let->combination exp)
  (let ((parameters (map assignment-var   (let-assignments exp)))
        (values     (map assignment-value (let-assignments exp))))
    (append (list (make-lambda parameters (let-body exp))) values)))

(define (let? exp) (eq? (car exp) 'let))
(define (let-assignments exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (first-assignment assignments) (car assignments))
(define (rest-assignment  assignments) (cdr assignments))
(define (assignment-var   assignment)  (car assignment))
(define (assignment-value assignment)  (cadr assignment))

(put 'eval '(let) (lambda (exp env) (eval (let->combination exp) env)))