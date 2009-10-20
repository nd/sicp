(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ...
        ((unless? exp)              (analyze (unless->if exp)))
        ..
        (else (error "Unknown expression type -- ANALYZE" exp))))

(define (unless? exp) (eq? (car exp) 'unless))
(define (unless-predicat exp) (cadr exp))
(define (unless-consequent exp) (caddr exp))
(define (unless-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (unless->if exp)
  (make-if (unless-predicat exp) (unless-alternative exp) (unless-consequent exp)))

;; even eli bendersky doesn't know the meaning of having unless or if as procedure, so do I