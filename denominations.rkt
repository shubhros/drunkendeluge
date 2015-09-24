(define (change-amount amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((< amount 0) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination k-o-c)
  (cond ((= k-o-c 1) 1)
        ((= k-o-c 2) 5)
        ((= k-o-c 3) 10)
        ((= k-o-c 4) 25)
        ((= k-o-c 5) 50)))

; Excercise 2.20 - Coin change using lists.

(define (cc_list amount coin_list)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (null? coin_list)) 0)
        (else
         (+ (cc_list amount (except-first-denomination coin_list))
            (cc_list (- amount (first-denomination-list coin_list))
                coin_list)))))

(define (first-denomination-list coin_list)
  (car coin_list))

(define (except-first-denomination coin_list)
  (cdr coin_list))

(define ind-coins (list 50 25 20 10 5))
(define us-coins (list 50 25 10 5 1))
(define us-coins-reverse (list 1 5 10 25 50))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

