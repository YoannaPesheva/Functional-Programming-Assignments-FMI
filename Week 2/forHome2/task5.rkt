#lang racket
(require math/number-theory)
#|
Two numbers are amicable if the sum of the divisors of one of them is equal
to the other. Define a predicate that checks whether two numbers are amicable.
|#

(define (amicable? x y)
  (= (divisor-sum x) (divisor-sum y))
  )

(equal? (amicable? 200 300) #f)
(equal? (amicable? 220 284) #t)
(equal? (amicable? 284 220) #t)
(equal? (amicable? 1184 1210) #t)
(equal? (amicable? 2620 2924) #t)
(equal? (amicable? 6232 6368) #t)