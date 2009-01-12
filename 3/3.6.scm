(define random-init 0)

(define (rand-update x)
  (+ x 1))

(define random
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate)          
             (begin                     
               (set! x (rand-update x)) 
               x))                      
            
            ((eq? m 'reset)             
             (lambda (new-value)             
               (set! x new-value)       
               x))))))
