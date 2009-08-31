(define (or-gate a b output)
  (let ((not-a (make-wire))
        (not-b (make-wire))
        (c (make-wire)))
    (inverter a not-a)
    (inverter b not-b)
    (and-gate not-a not-b c)
    (inverter c output)
    'ok))

;;or-delay = 2*inverter-delay + and-delay