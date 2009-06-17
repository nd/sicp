(define (make-account balance password)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Not enough money"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Wrong call -- make-account" m))))

  (define (dispatch-with-password p m)
    (if (eq? p password)
        (dispatch m)
        (lambda x "Wrong password")))

  dispatch-with-password)