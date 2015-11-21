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


;; QUEUE data structure using mutable lists

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

(define (print-queue queue)
  (front-ptr queue))



;; QUEUE data structure using ordinary lists and function states

(define (make-queue1)
  (let ((front-p '())
        (rear-p '()))
    (define (empty-queue?) (null? front-p))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-p new-pair)
               (set! rear-p new-pair))
              (else
               (set-cdr! rear-p new-pair)
               (set! rear-p new-pair)))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE called on empty queue"))
            (else
             (set! front-p (cdr front-p)))))
    (define (print-queue)
      front-p)
    (define (dispatch m)
      (cond ((eq? m 'empty-queue)
             (empty-queue?))
            ((eq? m 'insert-queue!)
             insert-queue!)
            ((eq? m 'delete-queue!)
             (delete-queue!))
            ((eq? m 'print-queue)
             (print-queue))))
    dispatch))

(define (insert-queue1 queue item)
  ((queue 'insert-queue!) item))

(define (delete-queue1 queue)
  (queue 'delete-queue!))

(define (print-queue1 queue)
  (queue 'print-queue))

(define (test-queue)
  (let ((q (make-queue1)))
    (insert-queue1 q 'a)
    (insert-queue1 q 'b)
    (insert-queue1 q 'c)
    (display (print-queue1 q))
    (display "\n")
    (delete-queue1 q)
    (display (print-queue1 q))
    (display "\n")
    (delete-queue1 q)
    (display (print-queue1 q))
    (display "\n")
    ))
