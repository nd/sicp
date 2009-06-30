;;append-map is the same as flatmap (defined in mzscheme)

(define (triples n s)
  (append-map
   (lambda (i)
     (append-map (lambda (j)
                   (append-map (lambda (k) (list (list i j k)))
                               (filter (lambda (k)
                                         (and (not (= i k))
                                              (not (= j k))
                                              (= (+ i j k)  s)))
                                       (enumerate-interval 1 n))))
                 (filter (lambda (j) (not (= i j)))
                         (enumerate-interval 1 n))))
   (enumerate-interval 1 n)))

