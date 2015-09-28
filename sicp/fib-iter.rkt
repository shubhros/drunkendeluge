(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (func n)
  (func-iter 2 1 0 n))

(define (func-iter a b c count)
  (if (< count 3)
      a
      (func-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
