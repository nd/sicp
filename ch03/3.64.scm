(define (stream-limit stream tolerance)
  (let ((first  (stream-car stream))
        (second (stream-car (stream-cdr stream))))
    (if (< (abs (- first second)) tolerance)
        second
        (stream-limit (stream-cdr stream) tolerance))))
