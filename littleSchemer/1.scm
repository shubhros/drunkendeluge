(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat? 
  (lambda (x)
    (cond ((null? x) #t)
          ((atom? (car x)) (lat? (cdr x)))
          (else
           #f))))
(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else
           (or (eq? (car lat) a)
               (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eq? a (car lat)) (cdr lat))
          (else
           (cons (car lat)
                 (rember a (cdr lat)))))))
(define firsts 
  (lambda (l)
    (cond ((null? l) '())
          ((atom? (car l)) '())
          (else
           (cons (car (car l))
                 (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old)
           (cons (car lat)
                 (cons new (cdr lat))))
          (else
           (cons (car lat)
                 (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old)
           (cons new lat))
          (else
           (cons (car lat)
                 (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old)
           (cons new (cdr lat)))
          (else
           (cons (car lat)
                 (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond ((null? lat) '())
          ((or (eq? (car lat) o1)
               (eq? (car lat) o2))
           (cons new (cdr lat)))
          (else
           (cons (car lat)
                 (subst2 new o1 o2 (cdr lat)))))))
(define multirember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eq? (car lat) a)
           (multirember a (cdr lat)))
          (else
           (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat))
           (cons 
            old 
            (cons 
             new 
             (multiinsertR new old (cdr lat)))))
          (else
           (cons (car lat)
                 (multiinsertR new old (cdr lat)))))))
           
(define multiinsertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old)
           (cons
            new 
            (cons 
             old
             (multiinsertL new old (cdr lat)))))
          (else
           (cons
            (car lat)
            (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old)
           (cons new
                 (multisubst new old (cdr lat))))
          (else
           (cons (car lat)
                 (multisubst new old (cdr lat)))))))

(define add1 1+)
(define sub1 1-)

(define plus
  (lambda (a b)
    (cond ((zero? b) a)
          (else
           (add1 (plus a (sub1 b)))))))

(define minus
  (lambda (a b)
    (cond ((zero? b) a)
          (else
           (sub1 (minus a (sub1 b)))))))

(define addtup
  (lambda (tup)
    (cond ((null? tup) 0)
          (else
           (+ (car tup)
              (addtup (cdr tup)))))))

(define mul
  (lambda (n m)
    (cond ((zero? m) 0)
          (else
           (plus n (mul n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond ((null? tup1) tup2)
          ((null? tup2) tup1)
          (else
           (cons 
            (+ (car tup1) (car tup2))
            (tup+ (cdr tup1) (cdr tup2)))))))

(define gt?
  (lambda (x y)
    (cond ((zero? x) #f)
          ((zero? y) #t) 
          (else
           (gt? (1- x) (1- y))))))
(define le?
  (lambda (x y)
    (cond ((zero? y) #f)
          ((zero? x) #t)
          (else
           (le? (1- x) (1- y))))))

(define eq
  (lambda (x y)
    (cond ((zero? x) (zero? y))
          ((zero? y) #f)
          (else
           (eq (1- x) (1- y))))))
(define eq-new
  (lambda (x y)
    (cond ((and (not (gt? x y)) (not (le? x y))) #t)
          (else #f))))

(define pow
  (lambda (x y)
    (cond ((zero? y) 1)
          (else
           (mul x (pow x (1- y)))))))
(define div
  (lambda (x y)
    (cond ((le? x y) 0)
          (else
           (1+ (div (minus x y) y))))))
