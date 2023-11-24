#lang racket
(require math/number-theory)
#|
A number is interesting if and only if it is evenly divided by the sum of its digits.
Define a predicate that checks whether a number is interesting.
|#

(define (sum-digits n)
  (if (zero? n)
      0
      (+ (sum-digits (quotient n 10)) (remainder n 10))
  )
)

(define (interesting? n)
 (divides? (sum-digits n) n)
)

(equal? (interesting? 410) #t)
(equal? (interesting? 212) #f)
(equal? (interesting? 567) #f)
(equal? (interesting? 70) #t)
(equal? (interesting? 5) #t)
(equal? (interesting? 4) #t)