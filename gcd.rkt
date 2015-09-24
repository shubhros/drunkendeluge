(define (gcd_1 a b)
  (if (= b 0)
      a
      (gcd_1 b (remainder a b))))
