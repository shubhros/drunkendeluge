; Auxillary functions

(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

(define (average x y z)
  (/ (+ x y z) 3))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeat f n)
  (if (= n 1)
      f
      (compose f (repeat f (- n 1)))))

(define (repeat-iter f n)
  (define (iter result state)
    (if (= state 1)
        result
        (iter (compose f result) (- state 1))))
  (iter f n))

(define (smooth f)
  (define dx 0.0000001)
  (lambda (x) (average (f x) (f (+ x dx)) (f (- x dx)))))

(define (n-smooth n f)
  ((repeat smooth n) f))
  