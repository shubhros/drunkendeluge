;Function to evaluate continuous fractions

(define (cont-frac-rec n d k)
  (define (calculate n d k state)
    (if (> state k)
        1
        (/ (n state) (+ (d state) (calculate n d k (+ state 1))))))
  (calculate n d k 1))


(define (cont-frac n d k)
  (define (iter state result)
    (if (= 0 state)
        result
        (iter (- state 1) (/ (n state) (+ (d state) result)))))
  (iter k 0))

(define main
  (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100))

(define (average-damp f)
  (define (average a b)
    (/ (+ a b) 2.0))
  (lambda (x) (average x (f x))))

(define (square x)
  (* x x))