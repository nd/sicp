(define (make-account balance password)

  (let ((wrong-password-calls 0))

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

    (define (call-the-cops . x)
      "Cops will be there in 5 minits")

    (define (dispatch-with-password p m)
      (if (eq? p password)
          (begin (set! wrong-password-calls 0)
                 (dispatch m))
          (if (>= wrong-password-calls 7)
              call-the-cops
              (begin (set! wrong-password-calls (+ wrong-password-calls 1))
                     (lambda x "Wrong password")))))

    dispatch-with-password))