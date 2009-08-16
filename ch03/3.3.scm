(define (make-account balance pswd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Not enough money"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch m p)
    (if (eq? p pswd)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit)  deposit)
              (else (error "Unknown call " m)))
        (error "wrong password")))

  dispatch)