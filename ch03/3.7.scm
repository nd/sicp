;;variant1 - both account has list of passwords:
(define (make-account balance pswd)
  (let ((pswds (list pswd)))

    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Not enough money"))

    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)

    (define (add-pswd new-pswd)
      (set! pswds (append pswds (list new-pswd)))
      dispatch)

    (define (dispatch m p)
      (if (memq p pswds)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit)  deposit)
                ((eq? m 'add-pswd) add-pswd)
                (else (error "Unknown call " m)))
          (error "wrong password")))

    dispatch))

(define (make-joint account pswd new-pswd)
  ((account 'add-pswd pswd) new-pswd))


;;variant2 - wrapper over account:
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
        (cond ((eq? m 'withdraw)   withdraw)
              ((eq? m 'deposit)    deposit)
              ((eq? m 'check-pswd) t)
              (else (error "Unknown call " m)))
        (error "wrong password")))

  dispatch)

(define (make-joint account pswd new-pswd)
  (if (account 'check-pswd pswd)
      (lambda (m p)
        (if (eq? p new-pswd)
            (account m pswd)
            (error "wrong password")))
      (error "wrong password of original account")))


;test:
(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
((peter-acc 'deposit 'open-sesame) 100)
((paul-acc 'withdraw 'rosebud) 100)
