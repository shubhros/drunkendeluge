(define (make-interval x y)
  (if (< x y)
      (cons x y)
      (cons y x)))

(define (lb z)
  (car z))

(define (ub z)
  (cdr z))

(define (add-interval x y)
  (make-interval (+ (lb x) (lb y)) 
                 (+ (ub x) (ub y))))

(define (mul-interval x y)
  (let ((p1 (* (lb x) (lb y)))
        (p2 (* (lb x) (ub y)))
        (p3 (* (ub x) (lb y)))
        (p4 (* (ub x) (ub y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (< (* (lb y) (ub y)) 0)
      (error "Cannot divide by an interval spanning zero" y)
      (mul-interval x (make-interval (/ 1.0 (ub y))
                                     (/ 1.0 (lb y))))))

(define (sub-interval x y)
  (make-interval (- (lb x) (ub y))
                 (- (ub x) (lb y))))
(define (width x)
  (/ (- (ub x) (lb x)) 2))

(define (make-center-percent c p)
  (let ((variance (/ (* c p) 100.0)))
    (make-interval (- c variance) (+ c variance))))

(define (center i)
  (/ (+ (lb i) (ub i)) 2.0))

(define (width i)
  (/ (- (ub i) (lb i)) 2.0))


(define z1 (make-interval 3 5))
(define z2 (make-interval -1 1))
(define z3 (make-center-percent 10 10))

