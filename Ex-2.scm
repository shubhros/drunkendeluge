;; ==================== Ex 2.2 - Points in a plane ============================

(define (make-point x y)
  (cons x y))

(define (x_cor point)
  (car point))

(define (y_cor point)
  (cdr point))

(define (print-point point)
  (newline)
  (display "(")
  (display (x_cor point))
  (display ", ")
  (display (y_cor point))
  (display ")")
  #t)

(define (dist p1 p2)
  (sqrt (+ (expt (- (x_cor p1) (x_cor p2)) 2)
		   (expt (- (y_cor p1) (y_cor p2)) 2))))

(define (make-seg point1 point2)
  (cons point1 point2))

(define (start-seg seg)
  (car seg))

(define (stop-seg seg)
  (cdr seg))

(define (mid-point segment)
  (define (average x y)
	(/ (+ x y) 2))
  (let ((start (start-seg segment))
		(end (stop-seg segment)))
	(make-point (average (x_cor start) (x_cor end))
				(average (y_cor start) (y_cor end)))))

;;=================== Ex 2.3 - Rectangles in a plane =========================

(define (make-rec p1 p2 p3 p4)
  (cons (cons (make-seg p1 p2) (make-seg p3 p4))
		(cons (make-seg p1 p4) (make-seg p2 p3))))

(define (len-rec rectangle)
  (let ((len-pair (car rectangle)))
	(let ((len-seg (car len-pair)))
	  (dist (start-seg len-seg)
			(stop-seg len-seg)))))

(define (br-rec rectangle)
  (let ((br-pair (cdr rectangle)))
	(let ((br-seg (car br-pair)))
	  (dist (start-seg br-seg)
			(stop-seg br-seg)))))

(define (perim-rec rectangle)
  (* 2 (+ (len-rec rectangle)
		  (br-rec rectangle))))

(define (area-rec rectangle)
  (* (len-rec rectangle) (br-rec rectangle)))

;;=================== What is data? =========================

; 1st way to implement pairs
(define (my_cons x y)
  (lambda (m)
	(cond ((= m 0) x)
		  ((= m 1) y))))

(define (my_car z)
  (z 0))

(define (my_cdr z)
  (z 1))

; 2nd way to implement pairs

(define (my_cons1 x y)
  (lambda (m) (m x y)))

(define (my_car1 z)
  (z (lambda (p q) p)))

(define (my_cdr1 z)
  (z (lambda (p q) q)))

; 3rd way to implement pairs

(define (my_cons2 x y)
  (define (dispatch m)
	(cond ((= m 0) x)
		  ((= m 1) y)))
  dispatch)

(define (my_car2 z)
  (z 0))
(define (my_cdr2 z)
  (z 1))


(define (my_cons3 x y)
  (* (expt 2 x)
	 (expt 3 y)))

(define (remove_factor result factor)
  (if (not (= 0 (remainder result factor)))
	  result
	  (remove_factor (/ result factor) factor)))

(define (my_car3 z)
  (sqrt (remove_factor z 3)))

(define (my_cdr3 z)
  (expt (remove_factor z 2) (/ 1 3)))

;; Numbers in terms of functions

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))


(define (add a b)
  (lambda (f) (lambda (x) ((b f) ((a f) x)))))


;================================================
;
; List Operations
;
;===============================================

(define (list-ref list count)
  (if (= count 0)
	  (car list)
	  (list-ref (cdr list) (- count 1))))

(define (list-len list)
  (if (null? list)
	  0
	  (+ 1 (list-len (cdr list)))))

(define (list-len-iter list)
  (define (iter list len)
	(if (null? list)
		len
		(iter (cdr list) (1+ len))))
  (iter list 0))


(define (my_append list1 list2)
  (if (null? list2)
	  list1
	  (my_append (cons list1 (car list2)) (cdr list2))))

(define (my_append2 list1 list2)
  (if (null? list1)
	  list2
	  (cons (car list1) (my_append2 (cdr list1) list2))))

(define (my_last_pair list)
  (if (null? (cdr list))
	  list
	  (my_last_pair (cdr list))))

(define (my_reverse l)
  (if (null? l)
	  '()
	  (my_append2 (my_reverse (cdr l)) (list (car l)))))


;; Coin counting problem using lists

(define (cc amount coin-values)
  (define (except-first-denomination coin-values)
	(cdr coin-values))
  (define (first-denomination coin-values)
	(car coin-values))
  (define no-more? null?)
  (cond ((= amount 0) 1)
		((or ( < amount 0) (no-more? coin-values)) 0)
		(else
		 (+ (cc amount
				(except-first-denomination coin-values))
			(cc (- amount
				   (first-denomination coin-values))
				coin-values)))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))


;; Find the numbers in the same parity in the 
;; list

(define (find-parity l result parity_check?)
  (cond ((null? l) (reverse result))
		((parity_check? (car l))
		 (find-parity (cdr l) (cons (car l) result) parity_check?))
		(else
		 (find-parity (cdr l) result parity_check?))))

(define (same-parity . y)
  (if (odd? (car y))
	  (find-parity y '() odd?)
	  (find-parity y '() even?)))


;; =================================================
;; Closure property and hierarchical data structures
;; =================================================
(define (my_map func list)
  (if (null? list)
	  '()
	  (cons (func (car list)) (my_map func (cdr list)))))

(define (scale-list list scale_factor)
  (my_map (lambda (x) (* scale_factor x)) list))


(define (square-list l)
  (if (null? l)
	  '()
	  (cons (square (car l)) (square-list (cdr l)))))


(define (square-list1 l)
  (map square l))


(define (square-list2 items)
  (define (iter things answer)
	(if (null? things)
		answer
		(iter (cdr things)
			  (cons (square (car things))
					answer))))
(iter items '() ))


(define (square-list3 items)
  (define (iter things answer)
	(if (null? things)
		answer
		(iter (cdr things)
			  (cons answer
					(square (car things))))))
  (iter items '() ))

(define (my_for_each l func)
  (cond ((null? l) #t)
		(else (func (car l))
			  (my_for_each (cdr l) func))))




(define (count-leaves tree)
  (cond ((null? tree) 0)
		((not (pair? tree)) 1)
		(else
		 (+ (count-leaves (car tree))
			(count-leaves (cdr tree))))))


; Ex - 2.27
; Deep reverse - reverse all embedded lists inside a lists
; Logic - if the source tree is null just return back an empty 
;         list. Else if first element in the list is again a 
;         list, deep-reverse it and add it in th front of the
;         deep-reversed rest of the list. Else add the element
;         in the front of the result list. Reverse the whole list
;

(define (deep-reverse tree)
  (reverse
   (cond ((null? tree) '())
		 ((pair? (car tree))
		  (cons (deep-reverse (car tree)) (deep-reverse (cdr tree))))
		 (else 
		  (cons (car tree) (deep-reverse (cdr tree)))))))

; Ex- 2.28
; fringe : Take a tree as an input and return a list whose elements
;          are all leaves of a tree arranged in left to write order
;
(define (fringe tree)
  (cond ((null? tree) '())
		((pair? tree)
		 (append (fringe (car tree)) (fringe (cdr tree))))
		(else
		 (list tree))))

; Ex - 2.29
; Binary mobiles

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))


(define level-1-mobile (make-mobile (make-branch 2 1) 
                                    (make-branch 1 2)))
(define level-2-mobile (make-mobile (make-branch 3 level-1-mobile) 
                                    (make-branch 9 1)))
(define level-3-mobile (make-mobile (make-branch 4 level-2-mobile) 
                                    (make-branch 8 2)))

(define (branch-weight branch)
  (let ((s (branch-structure branch)))
    (if (pair? s)
        (total-weight s)
        s)))

(define (total-weight mobile)
  (if (null? mobile)
      0
      (+ (branch-weight (left-branch mobile))
         (branch-weight (right-branch mobile)))))

(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (balanced-mobile? mobile)
  (= (branch-torque (left-branch mobile))
     (branch-torque (right-branch mobile))))

;
; Mapping over trees
;
(define (my-tree-map tree func)
  (cond ((null? tree) '())
		((not (pair? tree))
		 (func tree))
		(else
		 (cons 
		  (my-tree-map (car tree) func)
		  (my-tree-map (cdr tree) func)))))

(define (my-tree-map1 tree func)
  (map (lambda (sub-tree)
		 (if (pair? sub-tree)
			 (my-tree-map1 sub-tree func)
			 (func sub-tree)))
	   tree))


(define (my-tree-map2 tree func)
  (cond ((null? tree) '())
		((pair? tree)
		 (cons
		  (my-tree-map2 (car tree) func)
		  (my-tree-map2 (cdr tree) func)))
		(else (func tree))))

(define (subsets s)
  (if (null? s)
	  (list '())
	  (let ((rest (subsets (cdr s))))
		(append rest (map
					  (lambda (x)
						(cons (car s) x)) rest)))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
		((predicate (car sequence))
		 (cons (car sequence)
			   (filter predicate (cdr sequence))))
		(else
		 (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
	  initial
	  (op (car sequence)
		  (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
	  '()
	  (cons low
			(enumerate-interval (+ 1 low) high))))

(define (sum-of-odds low high)
  (accumulate + 0
			  (filter
			   odd?
			   (enumerate-interval low high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
		((pair? tree)
		 (append (enumerate-tree (car tree))
				 (enumerate-tree (cdr tree))))
		(else
		 (list tree))))

(define (sum-of-odd-leaves tree)
  (accumulate + 0
			  (filter even?
					  (enumerate-tree tree))))

(define (even-fibs n)
  (accumulate cons
			  '()
			  (filter even?
					  (map fib
						   (enumerate-interval 0 n)))))
					  

(define (list-fib-squares n)
  (accumulate cons
			  '()
			  (map (lambda (x) (* x x))
				   (map fib
						(enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
			  1
			  (map (lambda (x) (* x x))
				   (filter odd? sequence))))

(define (my_map_3 p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
			  '()
			  sequence))

(define (my_append_2 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my_length seq)
  (accumulate (lambda (x y)
				(1+ y))
				0
				seq))


(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
				(+ (* x higher-terms) this-coeff))
			  0
			  coefficient-sequence))

(define (count-leaves_1 tree)
  (accumulate +
			  0
			  (map (lambda (x) 1) (enumerate-tree tree))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
	  '()
	  (cons (accumulate op init 
						(map (lambda (x) (car x)) seqs))
			(accumulate-n op init
						  (map (lambda (x) (cdr x)) seqs)))))


;;;;;;;;;;;;;;;;;;;;;;;;;
; Matrix Multiplication;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define (dot-product u v)
  (accumulate + 0 (map * u v)))

(define (matrix-*-vector m v)
  (map (lambda (x) (accumulate + 0 x))
	   (map (lambda (x) (map * x v)) m)))

(define (transpose-matrix m)
  (accumulate-n cons '() m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose-matrix n)))
	(map
	 (lambda(x) (matrix-*-vector cols x))
	 m)))

(define fold-right accumulate)
(define (fold-left-rec op initial seq)
  (if (null? seq)
	  initial
	  (op (fold-left-rec op initial (cdr seq))
		  (car seq))))

(define (fold-left op init seq)
  (define (iter result rest)
	(if (null? rest)
		result
		(iter (op result (car rest))
			  (cdr rest))))
  (iter init seq))


;;
;; Define reverse in terms of fold-left and fold-right
;;
(define (fold-right-reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (fold-left-reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(define (generate-pairs n)
  (fold-left append '()
			  (map (lambda (i) (map (lambda (j) (list i j))
									(enumerate-interval 1 (- i 1))))
				   (enumerate-interval 1 n))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (generate-pairs-1 n)
  (flatmap (lambda(i)
			 (map (lambda (j)
					(list i j))
				  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n)))

(define (prime-sum? pair)
  (test-prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum-pair pair)
  (list (car pair) (cadr pair) (list (+ (car pair) (cadr pair)))))

(define (remove item s)
  (filter (lambda (x)
			(not (= item x)))
		  s))

(define (permutations seq)
  (if (null? seq)
	  (list '())
	  (flatmap (lambda (x)
				 (map (lambda (p)
					(cons x p)) (permutations (remove x seq))))
		   seq)))

(define (generate-pairs-2 n)
  (flatmap (lambda (x)
		 (map (lambda (y)
				(list x y))
			  (enumerate-interval 1 (- x 1))))
	   (enumerate-interval 1 n)))

(define (unique-pairs n)
  (filter (lambda (x)
			(> (car x) (cadr x)))
		  (generate-pairs n)))
			
		   
(define (prime-sum-pair n)
  (map make-pair-sum-pair
	   (filter prime-sum?
			   (unique-pairs n))))

(define (ordered-triplets n s)
  (filter (lambda (x)
            (= s (+ (car x)  (cadr x) (car (cddr x)))))
          (flatmap (lambda (x)
                     (flatmap (lambda (y)
                                (map (lambda (z)
                                       (list x y z))
                                     (enumerate-interval 1 (- y 1))))
                              (enumerate-interval 1 (- x 1))))
                   (enumerate-interval 1 n))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   SYMBOLIC DATA 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (memq? item seq)
  (cond ((null? seq) false)
		((eq? (car seq) item) true)
		(else (memq? item (cdr seq)))))


(define (equal-new? x1 x2)
  (cond ((and (null? x1) (null? x2)) true)
		((or (null? x1) (null? x2)) false)
		((and (pair? x1) (pair? x2))
		 (and (equal? (car x1) (car x2))
			  (equal? (cdr x1) (cdr x2))))
		((or (pair? x1) (pair? x2)) false)
		(else
		 (eq? x1 x2))))


; Derivates of sum and products
;
; Constructors and Selectors
;
(define (variable? x) (symbol? x))
(define (same-variable? x1 x2)
  (and (variable? x1) (variable? x2) (eq? x1 x2)))
(define (make-sum s1 s2)
  (cond ((eq? s1 '0) s2)
		((eq? s2 '0) s1)
		((and (number? s1) (number? s2)) (+ s1 s2))
		(else (list '+ s1 s2))))
(define (make-product m1 m2)
  (cond ((or (eq? m1 0) (eq? m2 0)) 0)
		((and (number? m1) (number? m2) (* m1 m2)))
		((eq? m1 1) m2)
		((eq? m2 1) m1)
		(else (list '* m1 m2))))
(define (sum? exp)
  (and (pair? exp) (eq? (car exp) '+)))
(define (product? exp)
  (and (pair? exp) (eq? (car exp) '*)))
(define (addend sum)
  (cadr sum))
(define (augend sum)
  (if (null? (cdddr sum))
	  (caddr sum)
	  (cons '+ (cddr sum))))
  
(define (multiplier product)
  (cadr product))
(define (multiplicand product)
  (if (null? (cdddr product))
	  (caddr product)
	  (cons '* (cddr product))))
(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '^)))
(define (base exp)
  (cadr exp))
(define (exponent exp)
  (caddr exp))
(define (make-exponentiation b e)
  (cond ((or (eq? b '1) (eq? e '0)) 1)
		((eq? e '1) b)
		(else (list '^ b e))))
;
; The derivative procedure
;

(define (deriv exp var)
  (cond ((number? exp) 0)
		((variable? exp)
		 (if (same-variable? exp var) 1 0))
		((sum? exp)
		 (make-sum (deriv (addend exp) var)
				   (deriv (augend exp) var)))
		((product? exp)
		 (make-sum
		  (make-product (multiplier exp) (deriv (multiplicand exp) var))
		  (make-product (multiplicand exp) (deriv (multiplier exp) var))))
		((exponentiation? exp)
		 (make-product
		  (make-product
		   (exponent exp) (make-exponentiation (base exp)
											   (make-sum (exponent exp)
														 -1)))
		  (deriv (base exp) var)))
		(else
		 (error "unknown expression type"))))

;;
; Infix notation derivative
;;
; Selectors and Constructors
;

(define (sum-infix? exp)
  (and (pair? exp) (eq? (cadr exp) '+)))
(define (product-infix? exp)
  (and (pair? exp) (eq? (cadr exp) '*)))
(define (make-sum-infix s1 s2)
  (cond ((eq? s1 '0) s2)
		((eq? s2 '0) s1)
		((and (number? s1) (number? s2)) (+ s1 s2))
		(else (list s1 '+ s2))))
(define (make-product-infix m1 m2)
  (cond ((or (eq? m1 0) (eq? m2 0)) 0)
		((and (number? m1) (number? m2) (* m1 m2)))
		((eq? m1 1) m2)
		((eq? m2 1) m1)
		(else (list m1 '* m2))))
(define (addend-infix sum)
  (car sum))
(define (augend-infix sum)
  (caddr sum))
(define (multiplier-infix product)
  (car product))
(define (multiplicand-infix product)
  (caddr product))


;;
;
; Representing Sets
;;
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))


(define (intersection-set set1 set2)
  (cond ((or (null? set1)
             (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

(define (adjoin-set-ordered x set)
  (cond ((null? set) (cons x '()))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        ((> x (car set)) (cons (car set)
                               (adjoin-set-ordered x (cdr set))))))


(define (union-set-ordered set1 set2)
  (cond ((null? set1) set2)
		((null? set2) set1)
		((= (car set1) (car set2))
		 (cons (car set1) (union-set-ordered (cdr set1) (cdr set2))))
		((> (car set1) (car set2))
		 (cons (car set2) (union-set-ordered set1 (cdr set2))))
		(else
		 (cons (car set1) (union-set-ordered (cdr set1) set2)))))

;;
;
; Sets as trees
;
;;

(define sample-tree 
  (list 3 
		(list 1 
			  '() 
			  '())
		(list 7 
			  (list 5 '() '()) 
			  (list 9 '() (list 11 '() '())))))
(define (entry tree)
  (car tree))
(define (left-br tree)
  (cadr tree))
(define (right-br tree)
  (caddr tree))
(define (make-tree entry left-br right-br)
  (list entry left-br right-br))

(define (element-of-set-tree? x set)
  (cond ((null? set) false)
		((= x (entry set)) true)
		((< x (entry set)) (element-of-set-tree? x (left-br set)))
		(else 
		 (element-of-set-tree? x (right-br set)))))

(define (adjoin-set-tree x set)
  (cond ((null? set) (make-tree x '() '()))
		((= x (entry set)) set)
		((< x (entry set))
		 (make-tree (entry set) 
					(adjoin-set-tree x (left-br set))
					(right-br set)))
		(else
		 (make-tree (entry set)
					(left-br set)
					(adjoin-set-tree x (right-br set))))))

(define (tree->list-1 tree)
  (if (null? tree)
	  '()
	  (append (tree->list-1 (left-br tree))
							(cons (entry tree)
								  (tree->list-1 (right-br tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result)
	(if (null? tree)
		result
		(copy-to-list (left-br tree)
					  (cons (entry tree)
							(copy-to-list (right-br tree)
										  result)))))
  (copy-to-list tree '()))


(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
					  remaining-elts))))))))


;
; Huffman Trees
;

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf leaf)
  (cadr leaf))

(define (weight-leaf leaf)
  (caddr leaf))

(define (make-code-tree left right)
  (list left
		right
		(append (symbols left) (symbols right))
		(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
	  (list (symbol-leaf tree))
	  (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
	  (weight-leaf tree)
	  (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
	(if (null? bits)
		'()
		(let ((next-branch (choose-branch (car bits) current-branch)))
		  (if (leaf? next-branch)
			  (cons (symbol-leaf next-branch)
					(decode-1 (cdr bits) tree))
			  (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
		((= bit 1) (right-branch branch))
		(else 
		 (error "bad bit-- CHOOSE-BRANCH"))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
		((< (weight x) (weight (car set))) (cons x set))
		(else
		 (cons (car set)
			   (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
	  '()
	  (let ((pair (car pairs)))
		(adjoin-set (make-leaf (car pair)  ;symbol
							   (cadr pair)) ;weight
					(make-leaf-set (cdr pairs))))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define sample-tree (make-code-tree (make-leaf 'A 4)
									(make-code-tree
									 (make-leaf 'B 2)
									 (make-code-tree 
									  (make-leaf 'D 1)
									  (make-leaf 'C 1)))))

(define sample-pairs (list '(A 4) '(B 2) '(C 1) '(D 1)))
(define sample-leaf-set (make-leaf-set sample-pairs))

(define (encode message tree)
  (if (null? message)
	  '()
	  (append (encode-symbol (car message) tree)
			  (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (encode-1 symbol tree result)
	(cond ((leaf? tree) result)
		  ((element-of-set? symbol (symbols (left-branch tree)))
		   (encode-1 symbol (left-branch tree) (append result '(0))))
		  (else
		   (encode-1 symbol (right-branch tree) (append result '(1))))))
  (define (encode-1-rec symbol tree)
	(cond ((leaf? tree) '())
		  ((element-of-set? symbol (symbols (left-branch tree)))
		   (cons 0 (encode-1-rec symbol (left-branch tree))))
		  (else
		   (cons 1 (encode-1-rec symbol (right-branch tree))))))
  
  (if (not (element-of-set? symbol (symbols tree)))
	  '()
	  (encode-1-rec symbol tree)))


;
; Generating a Huffman Tree
;

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


(define (successive-merge leaves)
  (if (null? (cdr leaves))
	  (car leaves)
	  (successive-merge
	   (adjoin-set (make-code-tree (car leaves) (cadr leaves))
				   (cddr leaves)))))

(define rock-pairs (list '(A 2) '(BOOM 1) '(GET 2) '(JOB 2) '(NA 16) '(SHA 3) '(YIP 9) '(WAH 1)))

(define rock-hf-tee (generate-huffman-tree rock-pairs))

;
; Multiple representations of abstract data
;

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
	  (car datum)
	  (error "Bad tagged datum - TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
	  (cdr datum)
	  (error "Bad tagged datum - CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
		   (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
		(real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-angle-rectangular r a)
  (attach-tag 'rectangular
			  (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z)
  (car z))

(define (angle-polar z)
  (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
			  (cons (sqrt (+ (square x) (square y)))
					(atan y x))))

(define (make-from-mag-angle-polar r a)
  (attach-tag 'polar (cons r a)))

(define (real-part z)
  (cond ((rectangular? z)
		 (real-part-rectangular (contents z)))
		((polar? z)
		 (real-part-polar (contents z)))
		(else
		 (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
		 (imag-part-rectangular (contents z)))
		((polar? z)
		 (imag-part-polar (contents z)))
		(else
		 (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
		 (magniude-rectangular (contents z)))
		((polar? z)
		 (magnitude-polar (contents z)))
		(else
		 (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectnagular? z)
		 (angle-rectangular (contents z)))
		((polar? z)
		 (angle-polar (contents z)))
		(else
		 (error "Unknown type -- ANGLE" z))))
		 

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
					   (+ (imag-oart z1) (imag-part z2))))
