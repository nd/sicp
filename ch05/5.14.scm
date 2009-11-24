(define (test-fact-machine)
  (map (lambda (n)
         (newline)
         (display (list 'n '= n))
         (test-fact-machine-for n))
       '(1 2 3 4 5 6 7 8 9 10)))

(define (test-fact-machine-for n)
  (let ((m (make-machine
           (list (list '* (lambda (x y) (* x y)))
                 (list '- (lambda (x y) (- x y)))
                 (list '= (lambda (x y) (= x y))))
           '(controller
             (assign continue (label fact-done))
             fact-loop
             (test (op =) (reg n) (const 1))
             (branch (label base-case))
             (save continue)
             (save n)
             (assign n (op -) (reg n) (const 1))
             (assign continue (label after-fact))
             (goto (label fact-loop))
             after-fact
             (restore n)
             (restore continue)
             (assign val (op *) (reg n) (reg val))
             (goto (reg continue))                
             base-case
             (assign val (const 1))               
             (goto (reg continue))                
             fact-done))))
    (set-register-contents! m 'n n)
    (m 'start)
    (newline)
    ((m 'stack) 'print-statistics)
    ((m 'instr-count))))

;;max-depth = n - 1
;;total-pushes = (n - 1) * 2