#lang racket
#|
Define a procedure that returns the number of occurrences
of a digit in a non-negative number.
|#

(define (count-occurrences n d)
  (define (helper copy-n d counter)
    (cond
      [(and (zero? n) (zero? d)) 1]
      [(zero? copy-n) counter]
      [(= (remainder copy-n 10) d) (helper (quotient copy-n 10) d (add1 counter))]
      [else (helper (quotient copy-n 10) d counter)]
      ) 
    )
  (if (negative? n)
      (error "The number must be non-negative")
      (helper n d 0)
      )
  )

(= (count-occurrences 121 1) 2)
(= (count-occurrences 222 1) 0)
(= (count-occurrences 100 0) 2)
(= (count-occurrences 0 0) 1)
