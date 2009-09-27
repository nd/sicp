(define (eval-and exp env)
  (define (iter exps)
    (let ((first-value (eval (first-exp exps) env)))
      (if (false? first-value)
          false
          (if (last-exp? exps)
              first-value
              (iter (rest-exps exps))))))
  (if (null? (and-expressions exp))
      true
      (iter (and-expressions exp))))

(define (eval-or exp env)
  (define (iter exps)
    (let ((first-value (eval (first-exp exps) env)))
      (if (true? first-value)
          first-value
          (if (last-exp? exps)
              false
              (iter (rest-exps exps))))))
  (if (null? (or-expressions exp))
      false
      (iter (or-expressions exp))))

(define (and? exp) (tagged-list? exp 'and))
(define (or?  exp) (tagged-list? exp 'or))
(define (and-expressions exp) (cdr exp))
(define (or-expressions  exp) (cdr exp))
(put 'eval '(and)  (lambda (exp env) (eval-and exp env)))
(put 'eval '(or)   (lambda (exp env) (eval-or  exp env)))

;;alternative approach

(define (and->if exp)
  (let ((exps (and-expressions exp)))
    (if (null? exps)
        true
        (expand-and->if exps))))

(define (expand-and->if exps)
  (let ((first (first-exp exps)))
    (if (last-exp? exps)
      (make-if first
               first
               false)
      (make-if first
               (expand-and->if (rest-exps exps))
               false))))

(define (or->if exp)
  (let ((exps (or-expressions exp)))
    (if (null? exps)
        false
        (expand-or->if exps))))

(define (expand-or->if exps)
  (let ((first (first-exp exps)))
    (if (last-exp? exps)
      (make-if first
               first
               false)
      (make-if first
               first
               (expand-or->if (rest-exps exps))))))