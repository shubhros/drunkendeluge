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
  (iter trials 0))


;; Mutable data structures - Mutable pairs

(define (my-append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (my-append (cdr list1) list2))))


(define (my-append! list1 list2)
  (set-cdr! (my-last-pair list1) list2)
  list1)

(define (my-last-pair x)
  (if (null? x)
      x
      (my-last-pair (cdr x))))


(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)


(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))


(define (set-to-wow! x)
  (set-car! (car x) 'wow))

(define (find-whether-unique p l)
  (cond ((not (pair? l)) #t)
        ((eq? p (car l)) #f)
        (else 
         (find-whether-unique p (cdr l)))))


(define (count-pairs l)
  (let ((gl '()))
    (define (helper x)
      (if 
       (or (not (pair? x))
           (memq x gl))
       0
       (begin
         (set! gl (cons x gl))
         (display gl)
         (display "\n")
         (display x)
         (display "\n")
         (+ (helper (car x))
            (helper (cdr x))
            1))))
    (helper l)))

(define (find-loop l)
  (let ((gl '()))
    (define (helper x)
      (if (not (pair? x))
          #f
          (if (memq x gl)
              #t
              (begin
                (set! gl (cons x gl))
                (display gl)
                (display "\n")
                (helper (cdr x))))))
      (helper l)))


(define (my-cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          ))
  dispatch)

(define (my-car z) (z 'car))
(define (my-cdr z) (z 'cdr))
(define (my-set-car! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (my-set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)
