(define (pe row col)
  (cond ((= col 0) 0)
        ((> col row) 0)
        ((= row 1) 1)
        (else (+ (pe (- row 1) (- col 1)) (pe (- row 1) col)))))
      
 

