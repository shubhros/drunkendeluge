; Return the kth fibonacci number
(define (fib k)
  (define (iter a b k)
    (if (= 0 k)
        b
        (iter b (+ a b) (- k 1))))
  (iter 1 0 k))

; Generate list of even numbered fibonacci numbers
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

; Generate list of fibonacci numbers
(define (fib-list n)
  (define (generate k)
    (if (> k n)
        '()
        (cons (fib k) (generate (+ k 1)))))
  (generate 0))

; Sum of squares of ODD leaves of a tree
(define sample-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(define (sum-odd-square tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) ((lambda (x) (* x x)) tree) 0))
        (else (+ (sum-odd-square (car tree))
                 (sum-odd-square (cdr tree))))))



(define items1 (list 1 2 3 4 5 6 7 8 9 10))
(define items2 (list 10 9 8 7 6 5 4 3 2 1))

;the filter procedure, based on a predicate
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

; Accumulate, using an operation
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

;Enumerate

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ 1 low) high))))

; Map reduce version of fib-even
(define (fib-even-mr n)
  (accumulate cons '() (filter even? (map fib (enumerate-interval 0 n)))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

; Map reduce version of sum-odd-square-tree
(define (sum-odd-square-tree-mr tree)
  (accumulate + 0 (map (lambda (x) (* x x)) (filter odd? (enumerate-tree tree)))))

; List of squares of Fibonacci numbers
(define (list-fib-squares n)
  (accumulate cons '() 
              (map (lambda (x) (* x x )) (map fib (enumerate-interval 0 n)))))

(define (square x) (* x x))
; Product of odd integers in a sequence
(define (product-odd-integers sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

; map using accumulate
(define (m_map p sequence)
  (accumulate (lambda (first already-accumulated) 
                (cons (p first) already-accumulated))'() sequence))
; append using accumulate
(define (m_append seq1 seq2)
  (accumulate cons seq2 seq1))
; length using accumulate
(define (m_length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


; Horner's rule
(define (horner-eval x coefficient-seq)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms))) 0 coefficient-seq))

; Count leaves using accumulate

(define (count-leaves t)
  (accumulate + 0 (map (lambda (node) (if (pair? node) (count-leaves node) 1)) t)))

(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; Matrix

(define sm '((1 2 3) (4 5 6) (7 8 9)))
(define sv-1 '(1 2 3))
(define sv-2 '(4 5 6))
(define sv-3 '(7 8 9))

; Dot product of two vectors

(define (dot-product u v)
  (accumulate + 0 (map * u v)))

(define (matrix-*-vector m v)
  (map (lambda (m-row) (dot-product m-row v)) m))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row) (matrix-*-vector n m-row)) m)))

(define (matrix-*-matrix-1 m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row)
           (map (lambda (n-cols) (dot-product n-cols m-row)) n)) m)))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (m_reverse seq)
  (accumulate (lambda (x y) (append y (list x))) '() seq))
(define (m_reverse-1 seq)
  (fold-left (lambda (x y) (append (list y) x)) '() seq))

; Generate ordered pairs i & j < n

;(define (generate-pairs n)
;  (accumulate append '()
;              (map (lambda (i)
;                     (map (lambda (j)
;                            (list i j))
;                          (enumerate-interval 1 (- i 1))))
;                   (enumerate-interval 1 n))))

(define (generate-pairs n)
  (flatmap (lambda (i) 
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

; Test for primes
(define (prime? n)
  (= n (smallest-divisor n)))

; Find the smallest divisor of n
(define (smallest-divisor n)
  (find-divisor n 2))
(define (divides? a b)
  (if (= 0 (remainder b a))
      #t
      #f))

; Auxillary routine to find divisors of n
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-divisor test-divisor)))))

(define (next-divisor n)
  (if (= n 2)
      3
      (+ n 2)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pair n)
  (map make-pair-sum
       (filter prime-sum? 
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (remove item seq)
  (filter (lambda (x) (not (= x item))) seq))

(define (permutations s)
  (if (null? s)
      '(())
      (map (lambda (x)
             (cons x (permutations (remove x s)))) s)))

(define (function n)
  (map (lambda (x)
         (list  (car x) (cadr x) (+ (car x) (cadr x))))
       (filter prime-sum?
               (flatmap 
                (lambda (i)
                  (map (lambda (j)
                         (list i j)) (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (permute set)
  (if (null? set)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p)
                        (cons x p)) (permute (remove x set)))) set)))

(define (subset s)
  (cond ((null? s) '())
        (else (let ((rest (subset (cdr s))))
                (append rest (map (lambda (x) (cons (car s) x)) rest))))))



(define (generate-triplets n)
  (flatmap (lambda (first)
         (flatmap (lambda (second)
                (map (lambda (third)
                       (list first second third)) (enumerate-interval 1 (- second 1))))
                (enumerate-interval 1 (- first 1))))
         (enumerate-interval 1 n)))

(define (generate-triplets-1 n)
  (flatmap (lambda (first)
         (flatmap (lambda (second)
                (map (lambda (third)
                       (list first second third)) (enumerate-interval 1 (- second 1))))
              (enumerate-interval 1 (- first 1))))
       (enumerate-interval 1 n)))



(define (generate-pairs-1 n)
  (map (lambda (first) (map (lambda (second)
                              (list first second))
                            (enumerate-interval 1 (- first 1))))
         (enumerate-interval 1 n)))