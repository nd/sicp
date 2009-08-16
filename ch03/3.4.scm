(define (make-account balance pswd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Not enough money"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (let ((error-count 0))
    (define (dispatch m p)
      (if (eq? p pswd)
          (begin
            (set! error-count 0)
            (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit)  deposit)
                (else (error "Unknown call " m))))
          (begin
            (set! error-count (+ error-count 1))
            (and (> error-count 7) (call-the-cops))
            (error "wrong password"))))

      dispatch))

(define (call-the-cops)
  (print "cops_are_called")
  (newline))