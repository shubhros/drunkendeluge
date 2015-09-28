(define (f x y)
  ((lambda (a b)
     (+ (* x ((lambda (x) (* x x)) a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (g x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x ((lambda (x) (* x x)) a))
       (* y b)
       (* a b))))

(define (k x)
  (let ((x 3)
        (y (+ x 2)))
    (* x y)))


