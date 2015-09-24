;------------------------------------------------
;       RATIONAL NUMBER PRIMITIVES
;------------------------------------------------

; Constructor
; Take numerator and denomin
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0) 
        (cons (* -1 (/ n g)) (* -1 (/ d g)))
        (cons (/ n g) (/ d g)))))
  
; Selectors 
(define numer car)
(define denom cdr)
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


; Test functionality

(define (rat-test x y)
  (print-rat (make-rat x y)))