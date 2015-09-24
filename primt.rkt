; Test for primes
(define (prime? n)
  (= n (smallest-divisor n)))

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



;Find the square of x
(define (square x)
  (* x x))

;Find the value of base^exp modulo m
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

;Fermat's little test for primality 
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;Fast prime check. Using Fermat's little theorem.
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


;Timed prime test. Find the time elapsed in searching for primes.
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (runtime) (current-milliseconds))