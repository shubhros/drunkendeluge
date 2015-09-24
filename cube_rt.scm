(define (sqrt_iter guess prev_guess x)
 (if (good_enough? guess prev_guess)
  guess
  (sqrt_iter (improve guess x) prev_guess x)))

(define (improve guess x)
 (/ (+ (* 2 guess) (/ x (square guess)) 3)))

(define (average x y)
 (/ (+ x y) 2))

(define (square x)
 (* x x))

(define (good_enough? guess prev_guess)
 (< (abs (- guess prev_guess)) 0.001))

(define (sq_rt x)
 (sqrt_iter 1.0 0.0 x))

