;; our compiler compute arguments from right to left
;; let's make it compute them from left to right

(load "compiler.scm")

(define (construct-arglist operand-codes)
  (if (null? operand-codes)
      (make-instruction-sequence
       '() '(argl)
       '((assign argl (const ()))))
      (let ((code-to-get-first-arg (append-instruction-sequences
                                    (car operand-codes)
                                    (make-instruction-sequence
                                     '(val) '(argl)
                                     '((assign argl (op list) (reg val)))))))
        (if (null? (cdr operand-codes))
            code-to-get-first-arg
            (preserving '(env)
                        code-to-get-first-arg
                        (code-to-get-rest-args (cdr operand-codes)))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
                     (car operand-codes)
                     (make-instruction-sequence
                      '(val argl) '(argl)
                      '((assign val (op list) (reg val))
                        (assign argl (op append) (reg argl) (reg val)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))

;;;;;result of factorial compilation:

((env) (val)
 ((assign val (op make-compiled-procedure) (label entry2) (reg env))
  (goto (label after-lambda1))

  entry2
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const =) (reg env))
  (assign val (op lookup-variable-value) (const n) (reg env)) ; n is calculated first
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign val (op list) (reg val))
  (assign argl (op append) (reg argl) (reg val)) ; argl is (n 1)
  (test (op primitive-procedure?) (reg proc)) 
  (branch (label primitive-branch17))

  compiled-branch16 
  (assign continue (label after-call15))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  primitive-branch17
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) ; (= n 1)

  after-call15
  (restore env)
  (restore continue)
  (test (op false?) (reg val)) 
  (branch (label false-branch4))

  true-branch5 ; return 1
  (assign val (const 1))
  (goto (reg continue))

  false-branch4 ; recursive factorial call
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const factorial) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const -) (reg env))
  (assign val (op lookup-variable-value) (const n) (reg env)) ; n
  (assign argl (op list) (reg val))
  (assign val (const 1)) ; 1
  (assign val (op list) (reg val))
  (assign argl (op append) (reg argl) (reg val)) ; argl is (n 1)
  (test (op primitive-procedure?) (reg proc)) ; - is primitive
  (branch (label primitive-branch8))

  compiled-branch7
  (assign continue (label after-call6))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  primitive-branch8
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

  after-call6
  (assign argl (op list) (reg val)) ; argl (- n 1)
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch11))

  compiled-branch10
  (assign continue (label after-call9))
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) ; recursive factorial call

  primitive-branch11
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

  after-call9
  (assign argl (op list) (reg val)) ; argl (factorial (- n 1))
  (restore env)
  (assign val (op lookup-variable-value) (const n) (reg env)) ; n
  (assign val (op list) (reg val))
  (assign argl (op append) (reg argl) (reg val)) ; argl is ((factorial (- n 1)) n)
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc)) ; * is primitive
  (branch (label primitive-branch14))

  compiled-branch13
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  primitive-branch14
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))

  after-call12
  after-if3
  after-lambda1
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))
  ))