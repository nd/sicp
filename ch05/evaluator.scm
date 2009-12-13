(load "../ch04/evaluator.scm")
(load "../ch04/4.8.scm")
(load "simulator.scm")

(define (get-global-environment) the-global-environment)

(define (last-operand? ops) (null? (cdr ops)))
(define (empty-arglist) '())
(define (adjoin-arg arg arglist) (append arglist (list arg)))

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
        (list 'let? let?)
        (list 'let->combination let->combination)
        (list 'prompt-for-input prompt-for-input)
        (list 'read read)
        (list 'get-global-environment get-global-environment)
        (list 'announce-output announce-output)
        (list 'user-print user-print)
        (list 'apply-primitive-procedure apply-primitive-procedure)
        (list 'rest-exps rest-exps)
        (list 'true? true?)
        (list 'false? false?)
        (list 'if-alternative if-alternative)
        (list 'definition-value definition-value)
        (list 'definition-variable definition-variable)
        (list 'car car)
        (list 'cdr cdr)
        (list 'null? null?)
        ))

(define eceval
  (make-machine
   (append build-in-operations
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
   '(
;;;======================
     read-eval-print-loop
;;;======================
     (perform (op initialize-stack))
     (assign the-stack (const ()))
     (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label eval-dispatch))
;;;==============
     print-result
;;;==============
     (perform (op announce-output) (const ";;; EC-Eval value:"))
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))
     
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
     (test (op let?) (reg exp))             (branch (label ev-let))
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

;;;========
     ev-let
;;;========
     (assign exp (op let->combination) (reg exp))
     (goto (label eval-dispatch))
     
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
     (save exp)
     (save env)
     (save continue)
     (save unev)
     (assign unev (op cond-clauses) (reg exp))
     (assign exp (op car) (reg unev))
     (goto (label ev-cond-clause))
;;;==============
     ev-cond-else
;;;==============
     (assign unev (op cdr) (reg unev))
     (test (op null?) (reg unev))
     (branch (label ev-cond-execute-clause-sequence))
     (goto (label else-in-the-middle-of-cond))
;;;================
     ev-cond-clause
;;;================
     (save exp)
     (save env)
     (save unev)
     (test (op cond-else-clause?) (reg exp))
     (branch (label ev-cond-else))
     (assign continue (label ev-cond-decide))
     (save continue)
     (assign exp (op cond-predicate) (reg exp))
     (goto (label eval-dispatch))
;;;================
     ev-cond-decide
;;;================
     (restore continue)
     (restore exp)
     (restore env)
     (restore unev)
     (test (op true?) (reg val))
     (branch (label ev-cond-execute-clause-sequence))
     (assign unev (op cdr) (reg unev))
     (assign exp (op car) (reg unev))
     (goto (label ev-cond-clause))
;;;===================
     ev-cond-execute-clause-sequence
;;;===================
     (assign unev (op cond-actions) (reg exp))
     (goto (label ev-sequence))


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


;;;=========================
     unknown-expression-type
;;;=========================
     (assign val (const unknown-expression-type-error))
     (goto (label signal-error))

;;;========================
     unknown-procedure-type
;;;========================
     (restore continue)    ; clean up stack (from `apply-dispatch')
     (assign val (const unknown-procedure-type-error))
     (goto (label signal-error))

;;;========================
     else-in-the-middle-of-cond
;;;========================
     (assign val (const else-in-the-middle-of-cond))
     (goto (label signal-error))     

;;;==============
     signal-error
;;;==============
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))
     )))

