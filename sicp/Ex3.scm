(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
           balance)
  "Insufficient funds")))

(define (make-account balance)
  (define (withdraw amount)
    (if (> balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)


;; Ex 3.3 - Accumulator

(define (make-accumulator sum)
  (define (increment-sum val)
    (begin (set! sum (+ sum val))
           sum))
  increment-sum)


(define (make-monitored f)
  (define (make-mon f num-calls)
    (define (mf arg)
      (cond ((eq? arg 'how-many-calls)
             num-calls)
            ((eq? arg 'reset-count)
             (set! num-calls 0))
            (else 
             (begin (set! num-calls (+ 1 num-calls))
                    (f arg)))))
    mf)
  (make-mon f 0))

;; Ex 3.4 - Secret password account program
(define (make-account-secret balance secret-password)
  (define wrong-password-count 0)
  (define (withdraw amount password)
      (if (eq? password secret-password)
          (if (> balance amount)
              (begin (set! balance (- balance amount))
                     balance)
              "Insufficient funds")
          (begin (set! wrong-password-count (+ 1 wrong-password-count))
                 (if (> wrong-password-count 2)
                    "Call the cops!"))))  
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else
           (error "Unknown request -- MAKE-ACCOUNT" m ))))
  dispatch)

;; Cesaro test

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0)))


