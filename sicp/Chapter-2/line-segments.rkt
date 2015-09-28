; Excercise - 2.2
; Represent line segments in a plane

; The point primitive
; A point is represented
; as (x,y)

;Constructor
(define (make-point x y)
  (cons x y))
;Selectors
(define (x-cord point)
  (car point))
(define (y-cord point)
  (cdr point))
; Operations on points
; Add two points as is
(define (add-point p_1 p_2)
  (make-point (+ (x-cord p_1) (x-cord p_2)) (+ (y-cord p_1) (y-cord p_2))))
; Divide the x and y coordinates of a point by scalar s
(define (scalar-div p s)
  (make-point (/ (x-cord p) s) (/ (y-cord p) s)))

; Distance formula - Calculate the distance between two
; points p_1 and p_2

(define (dist p_1 p_2)
  (sqrt (+ (expt (- (y-cord p_1) (y-cord p_2)) 2) (expt (- (x-cord p_1) (x-cord p_2)) 2))))

(define (slope p_1 p_2)
  (/ (- (y-cord p_2) (y-cord p_1)) (- (x-cord p_2) (x-cord p_1))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-cord p))
  (display " , ")
  (display (y-cord p))
  (display ")"))

; The line segment primitive
; A line segment is represented
; by two points in the X-Y plane
; (x_1,y_1), (x_2, y_2)

; Constructor
(define (make-segment p_1 p_2)
  (cons p_1 p_2))

; Selectors
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (print-segment segment)
  (print-point (start-segment segment))
  (print-point (end-segment segment)))

(define (mid-point-segment segment)
  (scalar-div (add-point (start-segment segment) (end-segment segment)) 2.0))
(define (slope_seg segment)
  (/ (- (y-cord (start-segment segment)) (y-cord (end-segment segment))) 
     (- (x-cord (start-segment segmnet)) (x-cord (end-segment segment)))))

; Excercise 2.3
; Primitives for a rectangle
