(define (double x)
  (+ x x))
(define (halve x)
  (/ x 2))
(define (mul a b)
  (iter 0 a b))

(define (iter x a b)
  (cond ((= b 1) a)
        ((even? b) (iter x (double a) (halve b)))
        (else (iter (+ x a) b (- b 1)))))

      