(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)  
                   (cddr exp))))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (empty-arglist) '())
(define (adjoin-arg arg arglist) (append arglist (list arg)))
(define (last-operand? ops) (null? (cdr ops)))

(define (make-procedure parameters body env) (list 'procedure parameters body env))
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))

(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define build-in-operations
  (list (list 'self-evaluating? self-evaluating?)
        (list 'variable? variable?)
        (list 'quoted? quoted?)
        (list 'text-of-quotation text-of-quotation)
        (list 'assignment? assignment?)
        (list 'assignment-variable assignment-variable)
        (list 'assignment-value assignment-value)
        (list 'definition? definition?)
        (list 'if? if?)
        (list 'if-predicate if-predicate)
        (list 'if-consequent if-consequent)
        (list 'lambda? lambda?)
        (list 'lambda-parameters lambda-parameters)
        (list 'lambda-body lambda-body)
        (list 'begin? begin?)
        (list 'begin-actions begin-actions)
        (list 'last-exp? last-exp?)
        (list 'first-exp first-exp)
        (list 'application? application?)
        (list 'operator operator)
        (list 'operands operands)
        (list 'no-operands? no-operands?)
        (list 'first-operand first-operand)
        (list 'rest-operands rest-operands)
        (list 'empty-arglist empty-arglist)
        (list 'adjoin-arg adjoin-arg)
        (list 'last-operand? last-operand?)
        (list 'make-procedure make-procedure)
        (list 'primitive-procedure? primitive-procedure?)
        (list 'compound-procedure? compound-procedure?)
        (list 'procedure-parameters procedure-parameters)
        (list 'procedure-body procedure-body)
        (list 'procedure-environment procedure-environment)
        (list 'lookup-variable-value lookup-variable-value)
        (list 'extend-environment extend-environment)
        (list 'set-variable-value! set-variable-value!)
        (list 'define-variable! define-variable!)
        (list 'cond? cond?)
        (list 'cond->if cond->if)
        (list 'cond-clauses cond-clauses)
        (list 'cond-else-clause? cond-else-clause?)
        (list 'cond-predicate cond-predicate)
        (list 'cond-actions cond-actions)
        (list 'expand-clauses expand-clauses)
        ))

(define evaluator
  (make-machine
   build-in-operations
   '(
;;;===============   
     eval-dispatch
;;;===============   
     (test (op self-evaluating?) (reg exp)) (branch (label ev-self-eval))
     (test (op variable?) (reg exp))        (branch (label ev-variable))
     (test (op quoted?) (reg exp))          (branch (label ev-quoted))
     (test (op assignment?) (reg exp))      (branch (label ev-assignment))
     (test (op definition?) (reg exp))      (branch (label ev-definition))
     (test (op if?) (reg exp))              (branch (label ev-if))
     (test (op cond?) (reg exp))            (branch (label ev-cond))
     (test (op lambda?) (reg exp))          (branch (label ev-lambda))
     (test (op begin?) (reg exp))           (branch (label ev-begin))
     (test (op application?) (reg exp))     (branch (label ev-application))
     (goto (label unknown-expression-type))

;;;==============
     ev-self-eval
;;;==============
     (assign val (reg exp))
     (goto (reg continue))

;;;=============
     ev-variable
;;;=============
     (assign val (op lookup-variable-value) (reg exp) (reg env))
     (goto (reg continue))

;;;===========
     ev-quoted
;;;===========
     (assign val (op text-of-quotation) (reg exp))
     (goto (reg continue))

;;;===========
     ev-lambda
;;;===========
     (assign unev (op lambda-parameters) (reg exp))
     (assign exp (op lambda-body) (reg exp))
     (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
     (goto (reg continue))

;;;================
     ev-application
;;;================
     (save continue)
     (save env)
     (assign unev (op operands) (reg exp))
     (save unev)
     (assign exp (op operator) (reg exp))
     (assign continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))
;;;======================
     ev-appl-did-operator
;;;======================
     (restore unev)                  ; the operands
     (restore env)
     (assign argl (op empty-arglist))
     (assign proc (reg val))         ; the operator
     (test (op no-operands?) (reg unev))
     (branch (label apply-dispatch))
     (save proc)
;;;======================
     ev-appl-operand-loop
;;;======================
     (save argl)
     (assign exp (op first-operand) (reg unev))
     (test (op last-operand?) (reg unev))
     (branch (label ev-appl-last-arg))
     (save env)
     (save unev)
     (assign continue (label ev-appl-accumulate-arg))
     (goto (label eval-dispatch))
;;;========================
     ev-appl-accumulate-arg
;;;========================
     (restore unev)
     (restore env)
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (assign unev (op rest-operands) (reg unev))
     (goto (label ev-appl-operand-loop))
;;;==================
     ev-appl-last-arg
;;;==================
     (assign continue (label ev-appl-accum-last-arg))
     (goto (label eval-dispatch))
;;;========================
     ev-appl-accum-last-arg
;;;========================
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (restore proc)
     (goto (label apply-dispatch))


;;;================   
     apply-dispatch
;;;================   
     (test (op primitive-procedure?) (reg proc))
     (branch (label primitive-apply))
     (test (op compound-procedure?) (reg proc))
     (branch (label compound-apply))
     (goto (label unknown-procedure-type))

;;;=================
     primitive-apply
;;;=================
     (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
     (restore continue)
     (goto (reg continue))

;;;================
     compound-apply
;;;================
     (assign unev (op procedure-parameters) (reg proc))
     (assign env (op procedure-environment) (reg proc))
     (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
     (assign unev (op procedure-body) (reg proc))
     (goto (label ev-sequence))


;;;==========
     ev-begin
;;;==========
     (assign unev (op begin-actions) (reg exp))
     (save continue)
     (goto (label ev-sequence))

;;;=============
     ev-sequence
;;;=============
     (assign exp (op first-exp) (reg unev))
     (test (op last-exp?) (reg unev))
     (branch (label ev-sequence-last-exp))
     (save unev)
     (save env)
     (assign continue (label ev-sequence-continue))
     (goto (label eval-dispatch))
;;;======================
     ev-sequence-continue
;;;======================
     (restore env)
     (restore unev)
     (assign unev (op rest-exps) (reg unev))
     (goto (label ev-sequence))
;;;======================
     ev-sequence-last-exp
;;;======================
     (restore continue)
     (goto (label eval-dispatch))


;;;=======
     ev-if
;;;=======
     (save exp)                    ; save expression for later
     (save env)
     (save continue)
     (assign continue (label ev-if-decide))
     (assign exp (op if-predicate) (reg exp))
     (goto (label eval-dispatch))  ; evaluate the predicate
;;;==============
     ev-if-decide
;;;==============
     (restore continue)
     (restore env)
     (restore exp)
     (test (op true?) (reg val))
     (branch (label ev-if-consequent))
;;;===================
     ev-if-alternative
;;;===================
     (assign exp (op if-alternative) (reg exp))
     (goto (label eval-dispatch))
;;;==================
     ev-if-consequent
;;;==================
     (assign exp (op if-consequent) (reg exp))
     (goto (label eval-dispatch))

;;;=========
     ev-cond
;;;=========
     (assign exp (op cond->if exp) (reg exp))
     (goto (label ev-if))
     

;;;===============
     ev-assignment
;;;===============
     (assign unev (op assignment-variable) (reg exp))
     (save unev)                   ; save variable for later
     (assign exp (op assignment-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-assignment-1))
     (goto (label eval-dispatch))  ; evaluate the assignment value
;;;=================
     ev-assignment-1
;;;=================
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))


;;;===============
     ev-definition
;;;===============
     (assign unev (op definition-variable) (reg exp))
     (save unev)                   ; save variable for later
     (assign exp (op definition-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))
     (goto (label eval-dispatch))  ; evaluate the definition value
;;;=================
     ev-definition-1
;;;=================
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op define-variable!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))
     )))
