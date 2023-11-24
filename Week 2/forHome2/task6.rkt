#lang racket
(require math/number-theory)
#|
Define a procedure sum-special-primes n d that returns the sum
of the first n prime numbers that contain the digit d.
|#

(define (contains? n d)  
  (cond
    [(zero? n) #f]
    [(= (remainder n 10) d) #t]
    [else (contains? (quotient n 10) d)]
    )
  )

(define (sum-special-primes n d)
  (define (helper copy-n iter sum)
    (cond
      [(zero? copy-n) sum]
      [(and (prime? iter) (contains? iter d)) (helper (sub1 copy-n) (add1 iter) (+ iter sum))]
      [else (helper copy-n (add1 iter) sum)]
    )
  )
  (helper n 2 0)
)

(= (sum-special-primes 5 2) 392)
(= (sum-special-primes 5 3) 107)
(= (sum-special-primes 10 3) 462)