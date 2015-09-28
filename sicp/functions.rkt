; Function to search for roots of an equation
(define (search f neg pos)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.0001))
  (let ((midpoint (average neg pos)))
    (if (close-enough? neg pos)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg midpoint))
                ((negative? test-value)
                 (search f midpoint pos))
                (else mid-point))))))


(define (average x y)
  (/ (+ x y) 2.0))

; Function to take find roots by successive halving.
(define (roots-by-halving f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (positive? a-value) (negative? b-value))
           (search f b a))
          ((and (negative? a-value) (positive? b-value))
           (search f a b))
          (else
           (error "Values are not of opposite sign" a b)))))

; Find fixed point of a function

(define tolerance 0.0000001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;(define (sqr_t x)
;  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(define gold
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(define new-fixed
  (fixed-point (lambda (x) (average (/ (log 1000) (log x)) x)) 2.0))
  ;(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))

(define (sqr_t x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (cube_rt x)
  (define (square a) (* a a))
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeat f n)
  (if (= n 1)
      f
      (compose f (repeat f (- n 1)))))

(define (fast-expn b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

(define (nth-root x n)
  (define (log2 n) (/ (log n) (log 2)))
  (fixed-point ((repeat average-damp (floor (log2 n))) (lambda (y) (/ x (expt y (- 1 n))))) 1.0))
;---------------------------------------------------------------------------
;                 NEWTON'S METHOD TO FIND ROOTS OF EQUATIONS
;---------------------------------------------------------------------------

(define (square x) (* x x))
(define dx 0.000001)
(define (deriv g)
  (lambda (x) 
    (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (test_root x)
  (newton-method (lambda (y) (- (square y) x)) 
                 1.0))
(define test_root2
  (newton-method (lambda (x) (+ (- (square x) (* 3 x)) 2)) 1.5)) 

(define (cubic a b c)
  (define (cube x) (* x x x))
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (cubic_root a b c)
  (newton-method (cubic a b c) 1.0))

(define (inc x)
  (+ x 1))

(define (double f)
  (lambda (x) (f (f x))))

(define (iterative-improve close-enough improve)
  (lambda (x) 
    (let ((xim (improve x)))
      (if (close-enough x xim)
          x
          ((iterative-improve close-enough improve) xim)))))

(define (iter-iterative-improve close-enough improve)
  (define (iter z)
    (let ((imz (improve z))) 
      (if (close-enough z imz)
          imz
          (iter imz))))
  (lambda (x) (iter x)))

(define (new-sqrt x)
  ((iter-iterative-improve close-enough? (lambda (y) (average y (/ x y)))) 1.0))

(define (new-fixed f first-guess)
  ((iterative-improve close-enough? f) first-guess))