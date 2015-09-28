(define (my_cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (my_car z) (z 0))
(define (my_cdr z) (z 1))


(define (new_cons x y)
  (lambda (m) (m x y)))

(define (new_car z)
  (z (lambda (p q) p)))

(define (new_cdr z)
  (z (lambda (p q) q)))

(define z (new_cons 4 5))

(define p (new_car z))

(define (arith_cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (count_powers z p)
  (define (iter state number)
    (if (= 0 (remainder number p))
        (iter (+ state 1) (/ number p))
        state))
  (iter 0 z))

(define (arith_car z)
  (count_powers z 2))

(define (arith_cdr z)
  (count_powers z 3))

(define pair (arith_cons 200 40))

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
