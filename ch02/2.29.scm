(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (make-branch length structure)
  (list length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (branch-weight branch)
  (let ((str (branch-structure branch)))
    (if (pair? str)
        (total-weight str)
        str)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-balanced? branch)
  (let ((str (branch-structure branch)))
    (or (not (pair? str))
        (mobile-balanced? str))))

(define (mobile-balanced? mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (= (* (branch-length left) (branch-weight left))
            (* (branch-length right) (branch-weight right)))
         (branch-balanced? left)
         (branch-balanced? right))))