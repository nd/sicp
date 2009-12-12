(load "simulator.scm")

;(define (append x y)
;  (if (null? x)
;      y
;      (cons (car x) (append (cdr x) y))))
(define (append-m x y)
  (let ((append-machine
         (make-machine
          (list (list 'car   car)
                (list 'cdr   cdr)
                (list 'cons  cons)
                (list 'null? null?))
          '((goto (label controller))

            append-func
            (restore x)
            (save x)   
            (restore y)
            (save y)
            (save tmp)
            ;;base case
            (test (op null?) (reg x))
            (branch (label base-case))
            ;;recursive call for cdr of the tree
            (assign continue (label after-cdr-appended))
            (save continue)
            (assign x (op cdr) (reg x))
            (save x)
            (save y)
            (goto (label append-func))

            after-cdr-appended
            (restore continue) ;;since we already here, remove one value of continue from the top of the stack
            (restore x);;refresh x
            (save x)
            (assign tmp (op car) (reg x))
            (assign val (op cons) (reg tmp) (reg val)) 
            (goto (label done))

            base-case
            (assign val (reg y))
            (goto (label done))

            done 
            (restore x)     ;;repair values of all used registers
            (restore y)
            (restore tmp)
            (restore continue);;this guarantee that continue value and 
            (save continue)   ;;top value on the stack for continue are the same
            (goto (reg continue))

            controller
            (assign continue (label end))
            (save continue)               
            (save x)                   
            (save y)                   
            (goto (label append-func))

            end))))
    (set-register-contents! append-machine 'x x)
    (set-register-contents! append-machine 'y y)
    (append-machine 'trace-on)
    (append-machine 'start)
    (newline)
    (newline)
    (display "val: ")
    (display (get-register-contents append-machine 'val))
    (newline)
    ((append-machine 'stack) 'print-statistics)))

;(define (append! x y)
;  (set-cdr! (last-pair x) y)
;  x)
; 
;(define (last-pair x)
;  (if (null? (cdr x))
;      x
;      (last-pair (cdr x))))
(define (append!-m x y)
  (let ((append-machine
         (make-machine
          (list (list 'car      car)
                (list 'cdr      cdr)
                (list 'cons     cons)
                (list 'set-cdr! set-cdr!)
                (list 'null?    null?))
          '((goto (label controller))

            last-pair-func
            (restore x)
            (save x)
            (save tmp)
            (assign tmp (op cdr) (reg x))
            (test (op null?) (reg tmp))
            (branch (label last-pair-base-case))
            (assign x (op cdr) (reg x))
            (save x)
            (goto (label last-pair-func))

            last-pair-base-case
            (assign val (reg x))
            (goto (label last-pair-done))

            last-pair-done
            (restore x)
            (restore tmp)
            (restore continue);;this guarantee that continue value and 
            (save continue)   ;;top value on the stack for continue are the same
            (goto (reg continue))

            append!-func
            (restore x)
            (save x)   
            (restore y)
            (save y)
            (save tmp)
            (assign continue (label after-last-pair))
            (save continue)
            (save x)
            (goto (label last-pair-func))

            after-last-pair
            (restore continue) ;;since we already here, remove one value of continue from the top of the stack
            (perform (op set-cdr!) (reg val) (reg y))
            (goto (label done))

            done 
            (restore x)     ;;repair values of all used registers
            (restore y)
            (restore tmp)
            (restore continue);;this guarantee that continue value and 
            (save continue)   ;;top value on the stack for continue are the same
            (goto (reg continue))

            controller
            (assign continue (label end))
            (save continue)               
            (save x)                   
            (save y)                   
            (goto (label append!-func))

            end))))
    (set-register-contents! append-machine 'x x)
    (set-register-contents! append-machine 'y y)
    (append-machine 'trace-on)
    (append-machine 'start)
    (newline)
    (newline)
    (display "val: ")
    (display (get-register-contents append-machine 'val))
    (newline)
    (display "x: ")
    (display (get-register-contents append-machine 'x))
    (newline)
    ((append-machine 'stack) 'print-statistics)))
