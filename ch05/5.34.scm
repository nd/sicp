((env)
 (val)
 ;; construct the procedure and skip over code for the procedure body
 ((assign val (op make-compiled-procedure) (label entry51) (reg env))
  (goto (label after-lambda50))
  
  entry51 ; calls to `factorial' will enter here
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  ;; construct the iter procedure and skip over code for the procedure body
  (assign val (op make-compiled-procedure) (label entry56) (reg env))
  (goto (label after-lambda55))

  entry56 ; calls to `iter' will enter here
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
  ;; begin actual procedure body
  (save continue)
  (save env)
  ;; compute `(> counter n)'
  (assign proc (op lookup-variable-value) (const >) (reg env))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch71))

  compiled-branch70
  (assign continue (label after-call69))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  primitive-branch71
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

  after-call69 ;val now contains result of (> counter n)
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch58))

  true-branch59
  (assign val (op lookup-variable-value) (const product) (reg env)) ; return product
  (goto (reg continue))

  ;; return (iter (* counter product) (+ counter 1))
  false-branch58
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (save continue)
  (save proc) ; save `iter' procedure
  (save env)
  (assign proc (op lookup-variable-value) (const +) (reg env)) ; compute (+ counter 1)
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch65))

  compiled-branch64
  (assign continue (label after-call63))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  primitive-branch65
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

  after-call63
  (assign argl (op list) (reg val)) ; now argl contain result of (+ counter 1)
  (restore env)
  (save argl) ; save partial arg list for `iter'
  (assign proc (op lookup-variable-value) (const *) (reg env)) ; compute (* counter product)
  (assign val (op lookup-variable-value) (const product) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch62))

  compiled-branch61
  (assign continue (label after-call60))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  primitive-branch62
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

  after-call60
  (restore argl) ; now val contain result of (* counter product)
  (assign argl (op cons) (reg val) (reg argl)) ; arg list for iter computed
  (restore proc) ; now proc contain `iter'
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch68))

  compiled-branch67
  (assign val (op compiled-procedure-entry) (reg proc)) ; now val = entry56
  (goto (reg val)) 

  primitive-branch68
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))

  after-call66
  after-if57
  after-lambda55 
  (perform (op define-variable!) (const iter) (reg val) (reg env)) ; define variable iter = compiled-procedure
  (assign val (const ok)) ; write 'ok to val
  (assign proc (op lookup-variable-value) (const iter) (reg env)) ; invoke iter with 1 1
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch54))

  compiled-branch53
  (assign val (op compiled-procedure-entry) (reg proc)) 
  (goto (reg val)) ; goto iter entry

  primitive-branch54
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))

  after-call52
  after-lambda50
  (perform (op define-variable!) (const factorial) (reg val) (reg env)) ; define procedure factorial
  (assign val (const ok)) ; write 'ok to val
  ))

;;the difference with recursive procedure is that we don't save
;;continue before recursive call, so we don't increment depth of the
;;stack on each recursive call