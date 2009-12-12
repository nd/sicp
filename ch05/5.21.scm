(load "simulator.scm")

(define (count-leaves structure)
  (let ((count-leaves-machine
         (make-machine
          (list (list '+     +)
                (list 'not   not)
                (list 'car   car)
                (list 'cdr   cdr)
                (list 'cons  cons)
                (list 'is-not-pair? (lambda (x) (not (pair? x))))
                (list 'null? null?)
                (list 'display display))
          '((goto (label controller))

            count-leaves   ;;procedure count-leaves
            (restore tree) ;;get value of argument tree
            (save tree)    ;;save all registers this procedure use for futher repair
            (save tmp)
            ;;first base case
            (test (op null?) (reg tree))
            (branch (label zero-case))
            ;;second base case
            (test (op is-not-pair?) (reg tree))
            (branch (label one-case))
            ;;recursive call for car of the tree
            (assign continue (label after-car-counted))
            (save continue)
            (assign tree (op car) (reg tree))
            (save tree)
            (goto (label count-leaves))

            after-car-counted
            ;;prepare for second recursive call
            (restore tree) ;; now register tree has value as it was before first call
            (save tree)    ;; save it for futher repair
            (assign tree (op cdr) (reg tree)) ;;argument for secound recursive call is cdr of tree
            (save tree)
            (save val)
            (assign val (const 0)) ;;make val 0 again
            (restore continue) ;;since we already here, remove one value of continue from the top of the stack
            (assign continue (label after-cdr-counted)) ;;after call continue from after-cdr-counted
            (save continue)
            (goto (label count-leaves))

            after-cdr-counted
            (restore continue) ;;since we already here, remove one value of continue from the top of the stack
            (assign tmp (reg val))
            (restore val)
            (assign val (op +) (reg val) (reg tmp)) 
            (goto (label count-leaves-done))

            one-case
            (assign val (const 1))
            (goto (label count-leaves-done))

            zero-case
            (assign val (const 0))
            (goto (label count-leaves-done))

            count-leaves-done ;;calculation done
            (restore tmp)     ;;repair values of all used registers
            (restore tree)
            (restore continue);;this guarantee that continue value and 
            (save continue)   ;;top value on the stack for continue are the same
            (goto (reg continue))

            controller
            (assign continue (label end)) ;;remember where to continue execution
            (save continue)               ;;after call to count-leaves     
            (save tree)                   ;;tree is an argument for count-leaves
            (goto (label count-leaves))

            end))))
    (set-register-contents! count-leaves-machine 'tree structure)
    (count-leaves-machine 'trace-on)
    (count-leaves-machine 'start)
    (newline)
    (newline)
    (display "val: ")
    (display (get-register-contents count-leaves-machine 'val))
    (newline)
    (display "tree: ")
    (display (get-register-contents count-leaves-machine 'tree))
    (newline)
    ((count-leaves-machine 'stack) 'print-statistics)))



;;b
(define (count-leaves-iter structure)
  (let ((count-leaves-machine
         (make-machine
          (list (list '+     +)
                (list 'not   not)
                (list 'car   car)
                (list 'cdr   cdr)
                (list 'is-not-pair? (lambda (x) (not (pair? x))))
                (list 'null? null?))
          '((goto (label controller))

            count-leaves   ;;procedure count-leaves
            (restore tree) ;;get value of argument tree
            (save tree)    ;;save all registers this procedure use for futher repair
            (restore val)
            ;;first base case
            (test (op null?) (reg tree))
            (branch (label zero-case))
            ;;second base case
            (test (op is-not-pair?) (reg tree))
            (branch (label one-case))
            ;;recursive call for car of the tree
            (assign continue (label after-car-counted))
            (save continue)
            (assign tree (op car) (reg tree))
            (save tree)
            (save val)
            (goto (label count-leaves))

            after-car-counted
            ;;prepare for second recursive call
            (restore tree) ;; now register tree has value as it was before first call
            (save tree)    ;; save it for futher repair
            (assign tree (op cdr) (reg tree)) ;;argument for secound recursive call is cdr of tree
            (save tree)
            (save val)
            (restore continue) ;;since we already here, remove one value of continue from the top of the stack
            (assign continue (label count-leaves-done)) ;;after call continue from after-cdr-counted
            (save continue)
            (goto (label count-leaves))

            one-case
            (assign val (op +) (reg val) (const 1))
            (assign continue (label count-leaves-done))
            (save continue)
            (goto (reg continue))

            zero-case
            (assign continue (label count-leaves-done))
            (save continue)
            (goto (reg continue))

            count-leaves-done ;;calculation done
            (restore continue);;remove one continue from stack
            (restore tree)    ;;repair values of all used registers
            (restore continue);;this guarantee that continue value and 
            (save continue)   ;;top value on the stack for continue are the same
            (goto (reg continue))

            controller
            (assign continue (label end)) ;;remember where to continue execution
            (save continue)               ;;after call to count-leaves     
            (save tree)                   ;;tree is an argument for count-leaves
            (assign val (const 0))
            (save val)
            (goto (label count-leaves))

            end))))
    (set-register-contents! count-leaves-machine 'tree structure)
    (count-leaves-machine 'trace-on)
    (count-leaves-machine 'start)
    (newline)
    (newline)
    (display "val: ")
    (display (get-register-contents count-leaves-machine 'val))
    (newline)
    (display "tree: ")
    (display (get-register-contents count-leaves-machine 'tree))
    (newline)
    ((count-leaves-machine 'stack) 'print-statistics)))


