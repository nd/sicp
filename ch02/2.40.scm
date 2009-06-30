(load "~/projects/sicp/ch02/accumulate.scm")

(define (enumerate-interval a b)
  (define (iter i result)
    (if (> i b)
        result
        (iter (+ i 1) (append result (list i)))))
  (iter a (list)))

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (flatmap (lambda (j) (list (list i j)))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 2 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))