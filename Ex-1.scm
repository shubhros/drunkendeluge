;; ================ Basic Exercises =======================

(define (sum-of-larger-numbers a b c)
  (cond ((and (>= a c) (>= b c)) (sum-of-squares a b))
		((and (>= b a) (>= c a)) (sum-of-squares b c))
		((and (>= c b) (>= a b)) (sum-of-squares a c))))

(define (square a) (* a a))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-iter x guess)
  (if (good-enough? guess x)
	  guess
	  (sqrt-iter x
				 (improve guess x))))

(define (good-enough? guess x)
  (< (abs(- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))


(define (crt-iter guess x)
  (if (good-enough-crt? guess x)
	  guess
	  (crt-iter (improve-crt guess x)
				x)))

(define (good-enough-crt? guess x)
  (< (abs (- (cube guess) x)) 0.0001))

(define (improve-crt guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))


(define (cube x)
  (* x x x))



;; ================  Coin Change Counting Problem =================

(define (count-change amount)
  (cc amount 5))

(define (cc amount kind-of-coins)
  (cond ((= amount 0) 1)
		((or (< amount 0) (= kind-of-coins 0)) 0)
		(else (+ (cc amount
					 (- kind-of-coins 1))
				 (cc (- amount (first-den kind-of-coins))
					 kind-of-coins)))))

(define (first-den kind-of-coins)
  (cond ((= kind-of-coins 1) 1)
		((= kind-of-coins 2) 2)
		((= kind-of-coins 3) 5)
		((= kind-of-coins 4) 10)
		((= kind-of-coins 5) 50)))


;; ======================== Pascal's Triangle =================

(define (pascal row column)
  (let ((arow (abs row))
		(acolumn (abs column)))
	(cond ((and (= arow 0) (= acolumn 0)) 1)
		  ((> acolumn arow) 0)
		  ((or (and (even? arow) (odd? acolumn))
			   (and (odd? arow) (even? acolumn))) 0)
		  (else
		   (+ (pascal (- arow 1) (- acolumn 1))
			  (pascal (- arow 1) (+ acolumn 1)))))))


;; ====================== Exponentiation ========================

(define (expt-rec b n)
  (if (= n 0)
	  1
	  (* b (expt-rec b (- n 1)))))

(define (expt-iter b counter product)
  (if (= 0 counter)
	  product
	  (expt-iter b (- counter 1)
				 (* b product))))

(define (fast-expt-rec b n)
  (cond ((= 0 n) 1)
		((even? n) (square (fast-expt-rec b (/ n 2))))
		(else (* b (fast-expt-rec b (- n 1))))))

(define (fast-expt-iter b counter product)
  (cond ((= 0 counter) product)
		((even? counter) (fast-expt-iter (square b) (/ counter 2)
										 product))
		(else (fast-expt-iter b (- counter 1)
							  (* b product )))))

(define (fe b n)
  (if (even? n)
	  (* b (fast-expt-iter b (- n 1) 1))
	  (fast-expt-iter b n 1)))

;;===================== Multiplication ====================

(define (mul a b)
  (if (= b 0)
	  0
	  (+ a (mul a (- b 1)))))

(define (mfr a b)
  (define (mul-fast a b)
	(cond ((= b 0) 0)
		  ((= b 1) a)
		  ((odd? b) (+ a (mul-fast a (- b 1))))
		  (else (mul-fast (* 2 a) (/ b 2)))))
  (mul-fast a b))

(define (mfi a b)
  (define (mul-fast-iter a b result)
	 (cond ((= b 0) result)
		   ((even? b) (mul-fast-iter (* 2 a) (/ b 2) result))
		   (else (mul-fast-iter a (- b 1) (+ result a)))))
  (mul-fast-iter a b 0))

;;============= Logarithmic time Fibonacci sequence =================

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
		((even? count)
		 (fib-iter a
				   b
				   (+ (* p p) (* q q))
				   (+ (* q q) (* 2 p q))
				   (/ count 2)))
		(else (fib-iter (+ (* b q) (* a q) (* a p))
						(+ (* b p) (* a q))
						p
						q
						(- count 1)))))


;; ======================= Test for primality ======================
;;            ==        FERMAT'S LITTLE THEOREM       ==
;; =================================================================

(define (expmod base exp m)
  (cond ((= exp 0) 1)
		((even? exp)
		 (remainder (square (expmod base (/ exp 2) m))
					m))
		(else
		 (remainder (* base (expmod base (- exp 1) m))
					m))))

(define (fermats-test n)
  (define (try-it a)
	(= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))


(define (fast-prime? n times)
  (cond ((= times 0) true)
		((fermats-test n) (fast-prime? n (- times 1)))
		(else false)))



;; ======================== Test of primality =================
;;                           General method 
;; ============================================================

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
		((= 0 (remainder n test-divisor)) test-divisor)
		(else (find-divisor n (+ 1 test-divisor)))))

(define (test-prime? n)
  (if (= n 1)
	  #f
	  (= n (smallest-divisor n))))


(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (test-prime? n)
	  (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


;; ==================== Higher order procedures =====================
; procedure sum - 

; Recursive process
(define (sum-rec a b term next)
  (if (> a b)
	  0
	  (+ (term a)
		 (sum (next a) b term next))))

; Iterative process
(define (sum-iter a b term next)
  (define (iter a b sum)
	(if (> a b)
		sum
		(iter (next a)
			  b 
			  (+ sum (term a)))))
  (iter a b 0))

; Sum of integers from 1 to n
(define (sum-int a b)
  (sum-iter a b (lambda (x) x) 1+))

; Sum of square of integers from 1 to n
(define (sum-sq a b)
  (sum-iter a b (lambda (x) (* x x)) 1+))


; procedure product

; recursive
(define (prod-rec a b term next)
  (if (> a b)
	  1
	  (* (term a)
		 (prod-rec (next a) b term next))))

; ietrative
(define (prod-iter a b term next)
  (define (iter a b prod)
	(if (> a b)
		prod
		(iter (next a) 
			  b
			  (* prod (term a)))))
  (iter a b 1))

(define (factorial n)
  (prod-rec 1 n (lambda (x) x) 1+))

;; Integral
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum-iter (+ a (/ dx 2)) b f add-dx)
	 dx))


;; Simpson's Rule -
(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* h k))))
  (define (term k)
	(if (odd? k)
		(* 4 (y k))
		(* 2 (y k))))
  (* (/ h 3.0) (sum-iter 1 n term 1+)))
		   


;;=================== Accumulation ========================

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
	  null-value
	  (combiner (term a)
				(accumulate-rec
				 combiner 
				 null-value 
				 term 
				 (next a) 
				 next 
				 b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a b result)
	(if (> a b)
		result
		(iter (next a)
			  b
			  (combiner result (term a)))))
  (iter a b null-value))

;; Filtered accumulate iterative	
(define (filtered-accumulate-iter combiner null-value term a next b filter)
  (define (iter a b result)
    (cond ((> a b) result)
          ((filter (term a)
                   (iter (next a) b (combiner result
                                              (val)))))
           (else 
            (iter (next a) b result ))))
    (iter a b null-value))
  
;; Filtered accumulate recursive
  (define (filtered-accumulate-rec combiner null-value term a next b filter)
  (cond ((> a b) null-value)
		((filter (term a))
		 (combiner (term a)
				   (filtered-accumulate-rec
					combiner
					null-value
					term
					(next a)
					next
					b
					filter)))
		(else
		 (filtered-accumulate-rec
					combiner
					null-value
					term
					(next a)
					next
					b
					filter))))
	  

(define (sum-accum a b term next)
  (accumulate-iter + 0 term a next b))

(define (sum-int-ac a b)
  (sum-accum a b (lambda (x) x) 1+))

(define (sum-prime a b)
  (filtered-accumulate-iter + 0 (lambda (x) x) a 1+ b test-prime?))


;; ================== Special forms lambda and let ======================

(define (f x y)
  ((lambda (a b)
	 (+ (* x (square a))
		(* y b)
		(* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f1 x y)
  (let ((a (+ 1 (* x y)))
		(b (- 1 y)))
	(+ (* x (square a))
	   (* y b)
	   (* a b))))
			   
;; =================== Roots of equation by successive averaging ==============

(define (search f pos-point neg-point tolerance)
  (define (close-enough? pos-point neg-point)
	(> tolerance (abs (- neg-point pos-point))))
  (let ((midpoint (/ (+ neg-point pos-point) 2)))
	(if (close-enough? pos-point neg-point)
		midpoint
		(let ((test-value (f midpoint)))
		  (cond ((positive? test-value)
				 (search f midpoint neg-point tolerance))
				((negative? test-value)
				 (search f pos-point midpoint tolerance))
				(else
				 midpoint))))))

(define (function x)
  (- (square x) 4))

;; ======================== Fixed of a function ==============================

(define (fixed-point f guess tolerance)
  (define (close-enough? v1 v2)
	(< (abs(- v1 v2)) tolerance))
  (define (try guess)
	(let ((next (f guess)))
	  (display guess)
	  (newline)
	  (if (close-enough? next guess)
		  next
		  (try next))))
  (try guess))

;; =============== Square root using fixed point =========================

(define (sqrt-fp x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
			   1.0
			   0.001))

;;===============  Golden Ratio using fixed point ==================

(define golden-ratio
  (fixed-point (lambda (x) (+ 1.0 (/ 1.0 x)))
			   1.0
			   0.00001))

;;================  Ex 1.36 ===========================

(define (average x y)
  (/ (+ x y) 2))

(define ex-1.36xs
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0 0.001))

;; ================= Ex 1.38 ========================

(define (cont-frac n d k)
  (define (frac n d k i)
	(if (= i k)
		(/ (d k) (n k))
		(/ (n i)
		   (+ (d i)
			  (frac n d k (+ 1 i))))))
  (frac n d k 1))

(define (cont-frac-iter n d k)
  (define (frac n d k result)
	(if (= k 0)
		result
		(frac n d (- k 1) (/ (n k) (+ (d k) result)))))
  (frac n d k 1))

(define ex-1.37a
  (cont-frac (lambda (x) 1.0)
			 (lambda (x) 1.0)
			 1000))

;; ===================== e value using continued fractions ====================

(define (d-euler x)
  (cond ((or (= x 1) (= x 3) (= x 4)) 1)
		((= x 2) 2)
		((= 0 (remainder x 3))
		 (+ (d-euler (- x 1)) (d-euler (- x 2)) (d-euler (- x 3))))
		(else 1)))
(define e-value
  (+ 2 (cont-frac (lambda (x) 1.0)
				  d-euler
				  100)))

;; ================= tan x using continued fraction ==========================

(define (tan-con x k)
  (cont-frac (lambda (i)
			   (if (= i 1)
				   x
				   (- (square x))))
			 (lambda (i)
			   (- (* 2 i) 1))
			 k))


;; =============== Procedures as return values ===============================

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (cube-rt x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
			   1.0
			   0.0001))


(define (deriv g)
  (define dx 0.00000001)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x)
	(- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g)
			   1.0
			   0.0001))

(define (sqrt-newton x)
  (newton-method (lambda (y) (- (square y) x))
				 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g)
			   guess
			   0.0001))

(define (sqrt-abs-1 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
							average-damp
							1.0))

(define (sqrt-abs-2 x) 
  (fixed-point-of-transform (lambda (y) (- (square y) x))
							newton-transform
							1.0))

;; ================= Ex 1.40 ================================

(define (cubic a b c)
  (lambda (x)
	(+ (cube x) (* a (square x)) (* b x) c)))

;; =================== Ex 1.41 ===============================

(define (double f)
  (lambda (x)
	(f (f x))))

;; =========================== Ex - 1.42 =======================

(define (compose f g)
  (lambda (x)
	(f (g x))))

;; ========================= Ex - 1.43 ==========================

(define (repeated f count)
  (if (= count 1)
	  f
	  (repeated (compose f f) (- count 1))))


;; ======================== Ex 1.45 ============================

(define (smooth f)
  (define dx 0.00001)
  (lambda (x)
	(/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

;; ====================== Ex 1.46 =============================

(define (iterative-improve good-enough? improve-guess)
  (lambda (guess)
	(define (iter guess)
	  (if (good-enough? guess)
		  guess
		  (iter (improve-guess guess))))
	(iter guess)))

(define (last-sqrt x)
  (define (good-enough? guess)
	(< (abs (- (square guess) x)) 0.01))
  (define (improve-guess guess)
	(average guess (/ x guess)))
  ((iterative-improve good-enough? improve-guess) 1.0))

(define (last-fixed-pt f)
  (define (good-enough? guess)
	(< (abs (- (f guess) guess)) 0.001))
  (define (improve-guess guess)
	(f guess))
  ((iterative-improve good-enough? improve-guess) 1.0))
