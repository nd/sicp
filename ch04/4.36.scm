(define (a-pythagorean-triples low)
  (let ((i (an-integer-starting-from low)))
    (let ((j (an-integer-starting-from i)))
      (let ((k (an-integer-between j (+ (* i i) (* j j)))))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

