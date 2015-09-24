(define (mymap p l)
  (define (iter p r l)
    (if (null? l)
	r
	(iter p
	      (cons (p (car l)) r)
	      (cdr l))))
  (reverse (iter p '() l)))

