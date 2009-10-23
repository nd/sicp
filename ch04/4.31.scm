(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure
                                    (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment (procedure-parameter-names procedure)
                                            (list-of-args procedure arguments env)
                                            (procedure-environment procedure))))
        (else (error "Unknown procedure type -- APPLY" procedure))))

(define (procedure-parameter-names p)
  (map (lambda (param)
         (if (list? param)
             (car param)
             param))
       (cadr p)))

(define (list-of-args procedure exps env)
  (define (iter params exps)
    (if (no-operands? exps)
      '()
      (let ((param (car params))
            (exp   (car exps)))
        (append
         (cond ((lazy-param? param) (list (delay-it exp env false)))
               ((lazy-memo-param? param) (list (delay-it exp env true)))
               (else (list (actual-value exp env))))
         (iter (cdr params) (cdr exps))))))
  (let ((params (procedure-parameters procedure)))
    (iter params exps)))

(define (lazy-param? param)
  (and (list? param) (eq? (cadr param) 'lazy)))

(define (lazy-memo-param? param)
  (and (list? param) (eq? (cadr param) 'lazy-memo)))

(define (delay-it exp env memo) (list 'thunk exp env memo)) 
(define (memoize-thunk? thunk) (cadddr thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (if (memoize-thunk? obj)
             (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
               (set-car! obj 'evaluated-thunk)
               (set-car! (cdr obj) result)
               (set-cdr! (cdr obj) '())   
               result)
             (actual-value (thunk-exp obj) (thunk-env obj))))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))

;;example:
;;(define lazy-count 0)
;;(define lazy-memo-count 0)
;;(define (lazyf x) (set! lazy-count (+ lazy-count 1)) x)
;;(define (lazy-memof x) (set! lazy-memo-count (+ lazy-memo-count 1)) x)
;;(define (f a (b lazy) c (d lazy-memo)) (* (+ a b c d) (+ a b c d)))
;; 
;;;;; L-Eval input:
;;(f 1 (lazyf 2) 3 (lazy-memof 4))
;; 
;;;;; L-Eval value:
;;100
;; 
;;;;; L-Eval input:
;;lazy-count
;; 
;;;;; L-Eval value:
;;2
;; 
;;;;; L-Eval input:
;;lazy-memo-count
;; 
;;;;; L-Eval value:
;;1