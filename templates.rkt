;Filtered accumulator
(define (filtered-accumulator combiner filter null-value manip next a b)
  (cond ((> a b) null-value)
        ((filter a) (combiner (manip a) (filtered-accumulator combiner filter null-value manip next (next a) b)))
        (else (combiner (manip null-value) (accumulator combiner filter null-value manip next (next a) b)))))
  
(define (accumulator combiner null-value manip next a b)
  (define (filter a)
    a)
  (filtered-accumulator combiner filter null-value manip next a b))

; Generic accumulator function
(define (accumulator-rec combiner null-value manip next a b)
  (if (> a b)
      null-value
      (combiner (manip a) (accumulator combiner null-value manip next (next a) b))))

; Generic iterarive accumulator
(define (accumulator-iter combiner null-value manip next a b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (manip a) result))))
  (iter a null-value))

; Sum using generic accumulator
(define (sum manip next a b)
  (accumulator + 0 manip next a b))

; Product using generic accumulator
(define (prod manip next a b)
  (accumulator * 1 manip next a b))

; Generic sum function to compute sum from a - b
(define (sum-rec manip next a b)
  (if (> a b)
      0
      (+ (manip a) (sum manip next (next a) b))))

; Iterative version of the sum from a - b
(define (sum-iter manip next a b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (manip a)))))
    (iter a 0))

; Some auxillary functions
(define (inc a)
  (+ 1 a))

; Function to sum integers from a - b
(define (sum-integers a b)
  (define (next a)
    (+ a 1))
  (define (manip a)
    a)
  (sum manip next a b))

; Function to sum cube of integers from a - b
(define (cube a)
  (* a a a))
(define (sum-cubes a b)
  (sum (lambda (x) (* x x x)) inc a b))

; Function to sum square of integers a - b
(define (square a)
  (* a a))
(define (sum-squares a b)
  (sum (lambda (x) (* x x)) inc a b))

;Function to calculate definite integral in range a - b

(define (integral f a b dx)
  (define (add-dx a)
    (+ a dx))
  (* (sum f add-dx (+ a (/ dx 2.0)) b)
     dx))

;Calculate integral using Simpson's rule
(define (simpson f a b n)
  (let ((h (/ (- b a) n)))
  (define (simpson-term k)
    (* (cond ((or (= 0 k) (= 1 k)) 1)
             ((odd? k) 4)
             (else 2)) (f (+ a (* h k)))))
  (* (/ h 3.0) (sum simpson-term inc 0 n))))

;-------------------------------------------------------------------------;

; Iterative product from a - b
(define (prod-iter manip next a b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (manip a)))))
  (iter a 1))

; Recursive prouct from a - b

(define (prod-rec manip next a b)
  (if (> a b)
      a
      (* (manip a) (prod manip next (next a) b))))

(define (factorial n)
  (define (manip a)
    a)
  (prod manip (lambda (x) (+ x 1)) 1 n))
