;;a
(define m (make-machine
           '(n continue val)

           (list (list '* (lambda (x y) (* x y)))
                 (list '- (lambda (x y) (- x y)))
                 (list '+ (lambda (x y) (+ x y)))
                 (list '= (lambda (x y) (= x y)))
                 (list '< (lambda (x y) (< x y))))

           '(controller
             (assign continue (label fib-done))

             fib-loop
             (test (op <) (reg n) (const 2))
             (branch (label immediate-answer))
             (save continue)
             (assign continue (label afterfib-n-1))
             (save n)                           
             (assign n (op -) (reg n) (const 1))
             (goto (label fib-loop))            

             afterfib-n-1                       
             (restore n)
             (assign n (op -) (reg n) (const 2))
             (assign continue (label afterfib-n-2))
             (save val)                         
             (goto (label fib-loop))

             afterfib-n-2                       
             (assign n (reg val))               
             (restore val)                      
             (restore continue)
             (assign val (op +) (reg val) (reg n))
             (goto (reg continue))              

             immediate-answer
             (assign val (reg n))               
             (goto (reg continue))

             fib-done)))

;;b
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set) (lambda (value) (set! contents value)))
            ((eq? message 'name) name)
            (else (error "Unknown request -- REGISTER" message))))
    dispatch))
(define (get-name register) (register 'name))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (list (get-name reg) (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (let ((top-of-stack (pop stack)))
        (if (eq? (car top-of-stack) (get-name reg))
            (begin
              (set-contents! reg (cadr top-of-stack))
              (advance-pc pc))
            (error "Wrong register" (get-name reg)))))))

;;c
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name) ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence) (assemble controller-text machine))
    (machine 'init-stack)
    machine))

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set) (lambda (value) (set! contents value)))
            ((eq? message 'name) name)
            (else (error "Unknown request -- REGISTER" message))))
    dispatch))
(define (get-name register) (register 'name))

;;if we use table for registers' stack we don't need initialize it
(define (make-stack)
  (let ((s (make-table)))
    (define (push reg)
      (let ((reg-stack ((s 'lookup) (list (get-name reg)))))
        (if reg-stack
            ((s 'insert!) (list (get-name reg)) (append (list (get-contents reg)) reg-stack))
            ((s 'insert!) (list (get-name reg)) (list (get-contents reg))))))
    (define (pop reg)
      (let ((reg-stack ((s 'lookup) (list (get-name reg)))))
        (if (or (null? reg-stack) (false? reg-stack))
            (error "Empty stack -- POP" (get-name reg))
            (let ((top (car reg-stack)))
              ((s 'insert!) (list (get-name reg)) (cdr reg-stack))
              top))))
    (define (initialize regs)
      (map (lambda (reg) ((s 'insert!) (list (get-name reg)) (list))) regs)
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push)       push)
            ((eq? message 'pop)        pop)
            ((eq? message 'initialize) initialize)
            ((eq? message 'contents)   s)
            (else (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop stack reg) ((stack 'pop) reg))
(define (push stack reg) ((stack 'push) reg))
(define (init-stack stack regs) ((stack 'initialize) regs))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((register-table (list (list 'pc pc) (list 'flag flag))))
      (let ((the-ops (list (list 'initialize-stack
                                 (lambda () (init-stack stack (map cadr register-table)))))))
        (define (allocate-register name)
          (if (assoc name register-table)
              (error "Multiply defined register: " name)
              (set! register-table (cons (list name (make-register name)) register-table)))
          'register-allocated)
        (define (lookup-register name)
          (let ((val (assoc name register-table)))
            (if val
                (cadr val)
                (error "Unknown register:" name))))
        (define (execute)
          (let ((insts (get-contents pc)))
            (if (null? insts)
                'done
                (begin
                  ((instruction-execution-proc (car insts)))
                  (execute)))))
        (define (dispatch message)
          (cond ((eq? message 'start)
                 (set-contents! pc the-instruction-sequence)
                 (execute))
                ((eq? message 'install-instruction-sequence) (lambda (seq) (set! the-instruction-sequence seq)))
                ((eq? message 'allocate-register) allocate-register)
                ((eq? message 'get-register) lookup-register)
                ((eq? message 'install-operations) (lambda (ops) (set! the-ops (append the-ops ops))))
                ((eq? message 'stack) stack)
                ((eq? message 'init-stack) (init-stack stack (map cadr register-table)))
                ((eq? message 'operations) the-ops)
                (else (error "Unknown request -- MACHINE" message))))
        dispatch))))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (push stack reg)
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack reg))
      (advance-pc pc))))

;;make-table from 3.25
(define (make-table . same-key?)
  (let ((local-table (list '*table*))
        (equal-p (if (null? same-key?) eq? same-key?)))

    (define (assoc key records)
      (cond ((null? records) false)
            ((equal-p key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (lookup keys)
      (define (iter keys table)
        (if (null? table)
            false
            (if (null? keys)
                table
                (if (list? table)
                    (let ((subtable (assoc (car keys) table)))
                      (if subtable
                          (iter (cdr keys) (cdr subtable))
                          false))
                    false))))
      (iter keys (cdr local-table)))

    (define (insert! keys value)
      (define (iter keys table)
        (if (null? keys)
            (set-cdr! table value)
            (if (list? (cdr table))
                (let ((subtable (assoc (car keys) (cdr table))))
                  (if subtable
                      (iter (cdr keys) subtable)
                      (let ((new-subtable (cons (car keys) '())))
                        (set-cdr! table (cons new-subtable (cdr table)))
                        (iter (cdr keys) new-subtable))))
                (let ((new-subtable (cons (car keys) '())))
                  (set-cdr! table (cons new-subtable '()))
                  (iter (cdr keys) new-subtable)))))
      (iter keys local-table)
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'insert!) insert!)
            ((eq? m 'lookup) lookup)
            ((eq? m 'local-table) (lambda () local-table))
            (else "unknown message " m)))

    dispatch))


;;===========
(define m (make-machine
           '(n continue)
           '()
           '(controller
             (assign continue (const 1))
             (save continue)
             (assign continue (const 3))
             (save continue)
             (assign n (const 2))
             (save n)                           
             (assign n (const 4))
             (save n)
             (restore continue)
             (restore continue))))