#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
(define one 0.99)
(define nil '())
(define origin (make-vect 0 0))
 
(define lower-right (make-vect one 0))
 
(define upper-left (make-vect 0 one))
 
(define upper-right (make-vect one one))

(define outline
  (segments->painter (list (make-segment origin lower-right)
                           (make-segment lower-right upper-right)
                           (make-segment upper-right upper-left)
                           (make-segment upper-left origin))))
(define (connect vect-list)
  (define (iter segment-list remaining)
    (if (null? (cdr remaining))
        (reverse segment-list)
        (iter (cons (make-segment (car remaining)
                                     (cadr remaining))
                       segment-list)
                 (cdr remaining))))
  (iter nil vect-list))

(connect (list origin
               lower-right
               upper-left
               upper-right))

(define (x-marks-the-spot frame)
  ((segments->painter (list (make-segment origin upper-right)
                            (make-segment lower-right upper-left)))
   frame))

(define (diamond frame)
  (let ((start (make-vect 0.5 0)))
    ((segments->painter (connect (list start
                                       (make-vect one 0.5)
                                       (make-vect 0.5 one)
                                       (make-vect 0 0.5)
                                       start)))
     frame)))


(define (wave frame)
  ((segments->painter (append (connect (list (make-vect 0.4  0.0)
                                             (make-vect 0.5  0.33)
                                             (make-vect 0.6  0.0))) ;inside legs
                              (connect (list (make-vect 0.25 0.0)
                                             (make-vect 0.33 0.5)
                                             (make-vect 0.3  0.6)
                                             (make-vect 0.1  0.4)
                                             (make-vect 0.0  0.6))) ;lower left
                              (connect (list (make-vect 0.0  0.8)
                                             (make-vect 0.1  0.6)
                                             (make-vect 0.33 0.65)
                                             (make-vect 0.4  0.65)
                                             (make-vect 0.35 0.8)
                                             (make-vect 0.4  1.0))) ;upper left
                              (connect (list (make-vect 0.75 0.0)
                                             (make-vect 0.6  0.45)
                                             (make-vect 1.0  0.15)));lower right
                              (connect (list (make-vect 1.0  0.35)
                                             (make-vect 0.8  0.65)
                                             (make-vect 0.6  0.65)
                                             (make-vect 0.65 0.8)
                                             (make-vect 0.6  1.0)))));upper right
   frame))

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define wave4-new (flipped-pairs wave))

(define (right-split painter n)
  (if (= 0 n)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
          (below top bottom))))

(define flipped-pairs-2
  (square-of-four identity flip-vert identity flip-vert))

(define (flipped-pairs-3 painter)
  (let ((combine4 (square-of-four identity flip-vert
                                identity flip-vert)))
    (combine4 painter)))

(define (square-limit-2 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  flip-horiz flip-vert)))
    (combine4 (corner-split painter n))))


(define (split first second)
