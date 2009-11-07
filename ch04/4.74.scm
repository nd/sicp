;;a
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map (lambda (s) (stream-car s))
              (stream-filter (lambda (s) (not (null? s))) stream)))
;;b