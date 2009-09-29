; ex:
; 
; (let ((a 0))
;   (while (< a 10)
;     (display a)))
;
; this is equal to:
;  
; (let ((a 0))
;   (let while ()
;     (if (< a 10)
;         (begin
;           (display a)
;           (set! a (+ a 1))
;           (while))
;         'ok)))

(define (while->let exp)
  (make-named-let 'while
                  '()
                  (make-if (while-condition exp)
                           (make-begin (append (while-body exp)
                                               (list '(while))))
                           'ok)))

(define (while? exp) (eq? (car exp) 'while))
(define (while-condition exp) (cadr exp))
(define (while-body exp) (cddr exp))

(put 'eval '(while) (lambda (exp env) (eval (while->let exp) env)))