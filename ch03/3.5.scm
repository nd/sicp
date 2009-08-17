(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y)))
  (let ((rect-area (* (- x2 x1) (- y2 y1))))
    (* rect-area 
       (monte-carlo trials experiment))))

(define (circle-predicat x y r)
  (define (predicat test-x test-y)
    (<= (+ (* (- test-x x) (- test-x x))
           (* (- test-y y) (- test-y y)))
        (* r r)))
  predicat)

;(estimate-integral (circle-predicat 0 0 1) -1.0 1.0 -1.0 1.0 100000)
;gives
;Value: 3.14244