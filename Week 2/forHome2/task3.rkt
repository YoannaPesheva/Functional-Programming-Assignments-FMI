#lang racket
(require math/number-theory)
 #| Define a recursive procedure that returns the sum of all
    prime divisors of a given number.
 |#

(define (sum-prime-divs-rec n)
  (define (helper iter sum)
    (cond
      [(> iter n) sum]
      [(and (prime? iter) (divides? iter n)) (helper (add1 iter) (+ sum iter))]
      [else (helper (add1 iter) sum)]
      )
    )
  (helper 2 0)
  )

(= (sum-prime-divs-rec 0) 0)
(= (sum-prime-divs-rec 6) 5) ; 2 + 3
(= (sum-prime-divs-rec 18) 5) ; 2 + 3
(= (sum-prime-divs-rec 19) 19)
(= (sum-prime-divs-rec 45136) 53)


