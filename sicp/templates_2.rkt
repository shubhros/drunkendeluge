;Filtered accumulator
(define (filtered-accumulator combiner filter null-value manip next a b)
  (cond ((> a b) null-value)
        ((filter a) (combiner (manip a) (filtered-accumulator combiner filter null-value manip next (next a) b)))
        (else (filtered-accumulator combiner filter null-value manip next (next a) b))))

; Identity function
(define (ident z)
  z)
; Incrementor
(define (inc x)
  (+ x 1))

; Square function
(define (square x)
  (* x x))

; Check if b is divisible by a
(define (divides? a b)
  (= 0 (remainder b a)))
;------------------------------------------------------------------------------
;                         PRIMALITY TESTING PREDICATE
;------------------------------------------------------------------------------
; Test for primes
(define (prime? n)
  (if (= 1 n)
      #f
      (= n (smallest-divisor n))))

; Find the smallest divisor of n
(define (smallest-divisor n)
  (find-divisor n 2))

(define (next-divisor n)
  (if (= n 2)
      3
      (+ n 2)))


; Auxillary routine to find divisors of n
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-divisor test-divisor)))))

;------------------------------------------------------------------------------
;                      SUM OF SQUARES OF PRIMES
;------------------------------------------------------------------------------

(define (sum-sqr-prime a b)
  (filtered-accumulator + prime? 0 square inc a b))


;------------------------------------------------------------------------------
;                    SUM OF RELATIVE PRIMES
;------------------------------------------------------------------------------

(define (sum-rel-prime n)
  (define (gcd-filter? a)
    (if (= 1 (gcd a n))
        true
        false))
  (filtered-accumulator + gcd-filter? 0 ident inc 1 n))
