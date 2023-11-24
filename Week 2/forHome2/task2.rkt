#lang racket
#| Define a linearly iterative procedure for calculating the sum
of the digits of a non-negative number. |#

(define (sum-digits-iter n)
  (define (helper copy-n sum)
    (if (zero? copy-n)
        sum
        (helper (quotient copy-n 10) (+ sum (remainder copy-n 10)))
        )
    )
  (if (negative? n)
      (error "The number must be non-negative")
      (helper n 0)
      )
  )

(= (sum-digits-iter 12345) 15)
(= (sum-digits-iter 123) 6)
;(sum-digits-iter -13) ; error "n was negative"
