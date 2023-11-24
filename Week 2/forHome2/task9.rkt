#lang racket
#|
Given a divisor d and a bound b, find the largest integer N, such that:
N is divisible by d
and N is less than or equal to b
and N is greater than 0.
|#

(define (max-multiple divisor bound)
  (define (helper iter divisor bound)
    (cond
      [(= iter 1) 1]
      [(zero? (remainder iter divisor)) iter]
      [else (helper (sub1 iter) divisor bound)]
    )
  )
  (helper bound divisor bound)
)

(= (max-multiple 2 7) 6)
(= (max-multiple 3 10) 9)
(= (max-multiple 7 17) 14)
(= (max-multiple 10 50) 50)
(= (max-multiple 37 200) 185)
(= (max-multiple 7 100) 98)