; TEST DATA
(define items1 (list 1 2 3 4 5 6 7 8 9 10))
(define items2 (list 1 2 3 4 5 6 7 8 9 10))
(define sample-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))


(define (m_list-ref s n)
  (if (= n 0)
      (car s)
      (m_list-ref (cdr s) (- n 1)))) 

(define (m_length s)
  (if (null? s)
      0
      (+ 1 (m_length (cdr s)))))

(define (m_length-iter s)
  (define (iter-len item count)
    (if (null? item)
        count
        (iter-len (cdr item) (+ 1 count))))
  (iter-len s 0))

(define (m_append s1 s2)
  (if (null? s1)
      s2
      (cons (car s1) (m_append (cdr s1) s2))))

(define (last-pair s)
  (if (null? (cdr (cdr s)))
      (cdr s)
      (last-pair (cdr s))))

(define (m_reverse s)
  (if (null? (cdr s))
      s
      (append (m_reverse (cdr s)) (list (car s)))))


; Excercise - 2.20

(define (same-parity x . y)
  (define (parity list rem)
    (cond ((null? list) list)
          ((= rem (remainder (car list) 2))
           (cons (car list) (parity (cdr list) rem) ))
          (else
           (parity (cdr list) rem))))
  (if (even? x)
      (parity y 0)
      (parity y 1)))

;; Map - Operation.

(define (m_map proc items)
  (if (null? items)
      items
      (cons (proc (car items)) (m_map proc (cdr items)))))

(define (square-list items)
  (m_map (lambda (x) (* x x)) items))

(define (square x) (* x x))
(define (square-list-1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))

(define (m-for-each proc items)
  (cond ((not (null? items))
         (proc (car items))
         (for-each proc (cdr items)))))

; Hierarchical Data
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

; Deep reverse
(define (deep-reverse x)
  (cond ((null? x) x)
        ((pair? (car x)) (append
                          (deep-reverse (cdr x))
                          (list (deep-reverse (car x)))))
        (else (append
               (deep-reverse (cdr x))
               (list (car x))))))

; Fringe

(define (fringe x)
  (cond ((null? x) x)
        ((pair? (car x)) (append (fringe (car x)) (fringe (cdr x))))
        (else (cons (car x) (fringe (cdr x))))))

; Binary Mobile


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
    (if (not (pair? s)) 
        s
        (total-weight s))))


(define (total-weight mobile)
  (cond ((null? mobile) 0)
        (else (+ (branch-weight (left-branch mobile))
                 (branch-weight (right-branch mobile))))))

(define (balanced? mobile)
  (= (left-torque mobile) (right-torque mobile)))

(define (left-torque mobile)
  (torque (left-branch mobile)))

(define (right-torque mobile)
  (torque (right-branch mobile)))

(define (torque branch)
  (* (branch-length branch) (branch-weight branch)))


; Mapping over trees

(define (scale-tree tree factor)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree-map tree factor)
  (m_map (lambda (sub-tree)
           (if (pair? sub-tree)
               (scale-tree-map sub-tree factor)
               (* sub-tree factor)))
         tree))

; Square nodes of a tree
(define (square-tree tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

; Use map, to square nodes of a tree
(define (square-tree-map tree)
  (m_map (lambda (sub-tree)
           (if (pair? sub-tree)
               (square-tree-map sub-tree)
               (square sub-tree))) tree))

; Map procedure for a tree
(define (tree-map proc tree)
  (m_map (lambda (sub-tree)
           (if (pair? sub-tree)
               (tree-map proc sub-tree)
               (proc sub-tree))) tree))

; Subsets of a SET
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

           

(define (m_add a b)
  (if (= 0 b)
      a
      (+ 1 (m_add a (- b 1)))))
           