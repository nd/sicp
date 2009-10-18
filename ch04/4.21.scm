;;a
((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (f n)
      (if (or (= n 1) (= n 2))
          1
          (+ (f f (- n 1)) (f f (- n 2)))))))
 10)

;;b
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))