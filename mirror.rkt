(define sample-tree (list (list 1 (list 2 (list 3 7))) (list 4 (list 5 (list 10 6)))))

(define (mirror tree)
  (cond ((null? tree) tree)
        ((list? (car tree)) (append (mirror (cdr tree)) (list (mirror (car tree)))))
        (else (append (mirror (cdr tree)) (list (car tree))))))
         