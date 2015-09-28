(define (my_cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1"))))
  dispatch)

(define (my_car z)
  (z 0))
(define (my_cdr z)
  (z 1))

(define (my_cons_1 x y)
  (lambda (m) (m x y)))

(define (my_car_1 z)
  (z (lambda (p q) p)))

(define (my_cdr_1 z)
  (z (lambda (p q) q)))