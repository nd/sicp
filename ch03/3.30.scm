(define (riple-carry-adder a b s c)
  (define (iter a b s c-in)
    (if (null? (cdr a))
        (full-adder (car a) (car b) c-in (car s) c)
        (let ((new-c-out (make-wire)))
          (full-adder (car a) (car b) c-in (car s) new-c-out)
          (iter (cdr a) (cdr b) (cdr s) new-c-out))))
  (let ((c-start (make-wire)))
    (set-signal! c-start 0)
    (iter a b s c-start)))

;; half_adder_delay = max(or_delay, and_delay + inv_delay) + and_delay
;; full_adder_delay = 2 * half_adder_delay + or_delay
;; riple_carry_delay = n * full_adder_delay = 2n * half_adder_delay + n * or_delay =
;; = 2n * max(or_delay, and_delay + inv_delay) + 2n * and_delay + n * or_delay