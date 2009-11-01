(define (queens board-size)
  (define (make-position row col) (list col row))
  (define (get-row position) (cadr position))
  (define (get-col position) (car position))
  (define (adjoin-position row col rest) (append rest (list (make-position row col))))
  (define empty-board (list))
  (define (safe? k positions)
    (let ((last-pos (last positions)))
      (let ((same-row-positions
             (filter (lambda (pos)
                       (and (< (get-col pos) k)
                            (= (get-row pos) (get-row last-pos))))
                     positions)))
        (let ((same-diag
               (filter (lambda (pos)
                         (and (< (get-col pos) k)
                              (= (/ (abs (- (get-row last-pos) (get-row pos)))
                                    (abs (- k (get-col pos))))
                                 1)))
                       positions)))
          (and (= (length same-row-positions) 0)
               (= (length same-diag) 0))))))
  (define (queen-cols k positions)
    (if (> k board-size)
        positions
        (let ((row (amb 1 2 3 4 5 6 7 8))
              (col (amb 1 2 3 4 5 6 7 8)))
          (require (safe? k positions))
          (adjoin-position row col positions))))
  (queen-cols 1 (empty-board)))