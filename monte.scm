(define (monte-carlo trials experiment)
  (define (iter remaining passed)
    (cond ((= remaining 0)
           (/ passed trials))
          ((experiment)
           (iter (- remaining 1)
                 (+ passed 1)))
          (else
           (iter (- remaining 1)
                 passed))))
  (iter trials 0))

(define (cesaro)
  (= (gcd (random 100) (random 100)) 1))


(define (estimate-pi n)
  (sqrt (/ 6 (monte-carlo n cesaro))))
