(load "../ch04/evaluator.scm")

(define (compile exp target linkage ct-env)
  (cond ((self-evaluating? exp) (compile-self-evaluating exp target linkage))
        ((quoted? exp)          (compile-quoted exp target linkage))
        ((variable? exp)        (compile-variable exp target linkage ct-env))
        ((assignment? exp)      (compile-assignment exp target linkage ct-env))
        ((definition? exp)      (compile-definition exp target linkage ct-env))
        ((if? exp)              (compile-if exp target linkage ct-env))
        ((lambda? exp)          (compile-lambda exp target linkage ct-env))
        ((begin? exp)           (compile-sequence (begin-actions exp) target linkage ct-env))
        ((cond? exp)            (compile (cond->if exp) target linkage ct-env))
        ((+? exp)               (compile-+ exp target linkage ct-env))
        ((-? exp)               (compile-- exp target linkage ct-env))
        ((=? exp)               (compile-= exp target linkage ct-env))
        ((*? exp)               (compile-* exp target linkage ct-env))
        ((application? exp)     (compile-application exp target linkage ct-env))
        (else (error "Unknown expression type -- COMPILE" exp))))

(define (+? exp) (tagged-list? exp '+))
(define (-? exp) (tagged-list? exp '-))
(define (=? exp) (tagged-list? exp '=))
(define (*? exp) (tagged-list? exp '*))

(define (compile-buildin-operation operation args defaults target linkage ct-env)
  (let ((args-code (spread-arguments args defaults ct-env)))
    (end-with-linkage
     linkage
     (append-instruction-sequences
      args-code
      (make-instruction-sequence
       '(arg1 arg2) (list target)
       `((assign ,target (op ,operation) (reg arg1) (reg arg2))))))))

(define (compile-+ exp target linkage ct-env)
  (let ((address (find-variable '+ ct-env)))
    (if (eq? address 'not-found)
        (let ((binary-exp (transform-to-binaries (operator exp) (operands exp))))
          (compile-buildin-operation '+ (operands binary-exp) '(0 0) target linkage ct-env))
        (compile-application exp target linkage ct-env))))

(define (compile-- exp target linkage ct-env)
  (let ((address (find-variable '- ct-env)))
    (if (eq? address 'not-found)
        (let ((binary-exp (transform-to-binaries (operator exp) (operands exp))))
          (compile-buildin-operation '- (operands binary-exp) '(0 0) target linkage ct-env))
        (compile-application exp target linkage ct-env))))

(define (compile-= exp target linkage ct-env)
  (let ((address (find-variable '= ct-env)))
    (if (eq? address 'not-found)
        (let ((binary-exp (transform-=-to-binaries (operands exp))))
          (compile-buildin-operation '= (operands exp)
                                     (let ((first (and (not (null? (operands binary-exp)))
                                                       (car (operands binary-exp)))))
                                       (if first
                                           (list first first)
                                           '(0 0)))
                                     target linkage ct-env))
        (compile-application exp target linkage ct-env))))

(define (compile-* exp target linkage ct-env)
  (let ((address (find-variable '* ct-env)))
    (if (eq? address 'not-found)
        (let ((binary-exp (transform-to-binaries (operator exp) (operands exp))))
          (compile-buildin-operation '* (operands binary-exp) '(1 1) target linkage ct-env))
        (compile-application exp target linkage ct-env))))

(define (transform-to-binaries op args)
  (if (<= (length args) 2)
      (cons op args)
      (let ((first-arg (car args))
            (second-arg (transform-to-binaries op (cdr args))))
        (list op first-arg second-arg))))

(define (transform-=-to-binaries args)
  (if (<= (length args) 2)
      (cons '= args)
      (let ((first-arg (car args))
            (second-arg (cadr args))
            (next-check (transform-=-to-binaries (cdr args))))
        (list 'and (list '= first-arg second-arg) next-check))))

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence
          '(continue) '()
          '((goto (reg continue)))))
        ((eq? linkage 'next) (empty-instruction-sequence))
        (else (make-instruction-sequence
               '() '()
               `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence
                     '() (list target)
                     `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence
                     '() (list target)
                     `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage ct-env)
  (let ((address (find-variable exp ct-env)))
    (if (eq? address 'not-found)
        (end-with-linkage linkage
                          (make-instruction-sequence
                           '(env) (list target)
                           `((assign ,target
                                     (op lookup-variable-value)
                                     (const ,exp)
                                     (reg env)))))
        (end-with-linkage linkage
                          (make-instruction-sequence
                           '(env) (list target)
                           `((assign ,target
                                     (op lexical-address-lookup)
                                     (const ,address)
                                     (reg env))))))))

(define (compile-assignment exp target linkage ct-env)
  (let* ((var (assignment-variable exp))
         (address (find-variable var ct-env))
         (get-value-code (compile (assignment-value exp) 'val 'next ct-env)))
    (if (eq? address 'not-found)
        (end-with-linkage linkage
                          (preserving
                           '(env)
                           get-value-code
                           (make-instruction-sequence
                            '(env val) (list target)
                            `((perform (op set-variable-value!)
                                       (const ,var)
                                       (reg val)
                                       (reg env))
                              (assign ,target (const ok))))))
        (end-with-linkage linkage
                          (preserving
                           '(env)
                           get-value-code
                           (make-instruction-sequence
                            '(env val) (list target)
                            `((perform (op lexical-address-set!)
                                       (const ,address)
                                       (reg val)
                                       (reg env))
                              (assign ,target (const ok)))))))))

(define (compile-definition exp target linkage ct-env)
  (let ((var (definition-variable exp)))
;    (define-variable! var var ct-env)
    (let ((get-value-code (compile (definition-value exp) 'val 'next ct-env)))
      (end-with-linkage linkage
                        (preserving
                         '(env)
                         get-value-code
                         (make-instruction-sequence
                          '(env val) (list target)
                          `((perform (op define-variable!)
                                     (const ,var)
                                     (reg val)
                                     (reg env))
                            (assign ,target (const ok)))))))))

(define (compile-if exp target linkage ct-env)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next ct-env))
            (c-code (compile (if-consequent exp) target consequent-linkage ct-env))
            (a-code (compile (if-alternative exp) target linkage ct-env)))
        (preserving
         '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence
           '(val) '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

(define (compile-sequence seq target linkage ct-env)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage ct-env)
      (preserving '(env continue)
                  (compile (first-exp seq) target 'next ct-env)
                  (compile-sequence (rest-exps seq) target linkage ct-env))))

(define (compile-lambda exp target linkage ct-env)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
                          (make-instruction-sequence
                           '(env) (list target)
                           `((assign ,target
                                     (op make-compiled-procedure)
                                     (label ,proc-entry)
                                     (reg env)))))
        (compile-lambda-body exp proc-entry ct-env))
       after-lambda))))

(define (compile-lambda-body exp proc-entry ct-env)
  (let* ((formals (lambda-parameters exp))
         (extended-ct-env (extend-environment formals formals ct-env)))
    (append-instruction-sequences
     (make-instruction-sequence
      '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (scan-out-defines (lambda-body exp)) 'val 'return extended-ct-env))))


(define label-counter 0)
(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)
(define (make-label name)
  (string->symbol
   (string-append (symbol->string name)
                  (number->string (new-label-number)))))

(define (make-compiled-procedure entry env) (list 'compiled-procedure entry env))
(define (compiled-procedure? proc) (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

(define (compile-application exp target linkage ct-env)
  (let ((proc-code (compile (operator exp) 'proc 'next ct-env))
        (operand-codes (map (lambda (operand) (compile operand 'val 'next ct-env)) (operands exp))))
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence
         '() '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg (append-instruction-sequences
                                     (car operand-codes)
                                     (make-instruction-sequence
                                      '(val) '(argl)
                                      '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
                          code-to-get-last-arg
                          (code-to-get-rest-args
                           (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
                     (car operand-codes)
                     (make-instruction-sequence
                      '(val argl) '(argl)
                      '((assign argl (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch  (make-label 'compiled-branch))
        (compound-branch  (make-label 'compound-branch))
        (after-call       (make-label 'after-call)))
    (let ((compiled-linkage (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
        '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))
          (test (op compound-procedure?) (reg proc))
          (branch (label ,compound-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (parallel-instruction-sequences
         (append-instruction-sequences
          compound-branch
          (compound-proc-appl target compiled-linkage))
         (append-instruction-sequences
          primitive-branch
          (end-with-linkage linkage
                            (make-instruction-sequence
                             '(proc argl)
                             (list target)
                             `((assign ,target
                                       (op apply-primitive-procedure)
                                       (reg proc)
                                       (reg argl))))))))
       after-call))))

(define all-regs '(env proc val argl continue))
(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence
          '(proc) all-regs
          `((assign continue (label ,linkage))
            (assign val (op compiled-procedure-entry)
                    (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence
            '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry)
                      (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue) all-regs
          '((assign val (op compiled-procedure-entry)
                    (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE" target))))

(define (compound-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence
          '(proc) all-regs
          `((assign continue (label ,linkage))
            (save continue)
            (goto (reg compapp)))))
        ((and (not (eq? target 'val)) (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence
            '(proc) all-regs
            `((assign continue (label ,proc-return))
              (save continue)
              (goto (reg compapp))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue) all-regs
          `((save continue)
            (goto (reg compapp)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE" target))))

(define (spread-arguments args defaults ct-env)
  (let* ((first-arg  (or (and (not (null? args))
                              (car args)) (car defaults)))
         (second-arg (or (and (not (null? args))
                              (not (null? (cdr args)))
                              (cadr args)) (cadr defaults)))
         (first-code (preserving '(arg2)
                      (compile first-arg 'arg1 'next ct-env)
                      (make-instruction-sequence
                       '(arg2) '() '())))
         (second-code (compile second-arg 'arg2 'next ct-env)))
    (preserving '(env)
                second-code
                first-code)))


(define (registers-needed s) (if (symbol? s) '() (car s)))
(define (registers-modified s) (if (symbol? s) '() (cadr s)))
(define (statements s) (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg) (memq reg (registers-needed seq)))

(define (modifies-register? seq reg) (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
                        (make-instruction-sequence
                         (list-union (list first-reg)
                                     (registers-needed seq1))
                         (list-difference (registers-modified seq1)
                                          (list first-reg))
                         (append `((save ,first-reg))
                                 (statements seq1)
                                 `((restore ,first-reg))))
                        seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

(define (make-instruction-sequence needs modifies statements) (list needs modifies statements))
(define (empty-instruction-sequence) (make-instruction-sequence '() '() '()))

(define (frame-number address) (car address))
(define (displacement-number address) (cdr address))
(define (make-lexical-address frame-number displacement-number) (cons frame-number displacement-number))

(define (lexical-address-lookup address env)
  (define (get-frame frame-number env)
    (if (= frame-number 0)
        (first-frame env)
        (get-frame (- frame-number 1) (enclosing-environment env))))
  (define (get-value-by-displacement values displacement-number)
    (if (= displacement-number 0)
        (car values)
        (get-value-by-displacement (cdr values) (- displacement-number 1))))
  (let* ((frame (get-frame (frame-number address) env))
         (value (get-value-by-displacement (frame-values frame) (displacement-number address))))
    (if (eq? value '*unassigned*)
        (error "Unassigned variable at address " address)
        value)))

(define (lexical-address-set! address value env)
  (define (get-frame frame-number env)
    (if (= frame-number 0)
        env
        (get-frame (- frame-number 1) (enclosing-environment env))))
  (define (set-value-by-displacement! values displacement-number val)
    (if (= displacement-number 0)
        (set-car! values val)
        (set-value-by-displacement! (cdr values) (- displacement-number 1) val)))
  (let* ((frame (get-frame (frame-number address) env)))
    (set-value-by-displacement! (frame-values frame) (displacement-number address) value)))

(define (find-variable var env)
  (define (env-loop env frame-number)
    (define (scan vars displacement)
      (cond ((null? vars)
             (env-loop (enclosing-environment env) (+ frame-number 1)))
            ((eq? var (car vars))
             (make-lexical-address frame-number displacement))
            (else (scan (cdr vars) (+ displacement 1)))))
    (if (eq? env the-empty-environment)
        'not-found
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) 0))))
  (env-loop env 0))

(define (scan-out-defines body)
  (let* ((defs (filter definition? body))
         (non-defs (filter (lambda (x) (not (definition? x))) body))
         (assignments (map (lambda (def) (make-let-assignment (definition-variable def) ''*unassigned*)) defs))
         (sets (map (lambda (def) (make-assignment (definition-variable def) (definition-value def))) defs)))
    (if (not (null? defs))
        (list (let->combination (make-let assignments (append sets non-defs))))
        body)))