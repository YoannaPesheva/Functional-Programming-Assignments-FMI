#lang racket
#| Define a recursive and an iterative procedure for calculating the
number of digits of a non-negative number.|#

; Iterative procedure
(define (count-digits-iter n)
  (define (helper copy-n count)
    (if (zero? copy-n)
        count
        (helper (quotient copy-n 10) (add1 count))
        )
    )
  (if (negative? n)
      (error "The number must be non-negative")
      (helper n 0)
      )
  )

; Recursive procedure
(define (count-digits-rec n)
  (cond
    [(negative? n) (error "The number must be non-negative")]
    [(zero? n) 0]
    [else (add1 (count-digits-rec(quotient n 10)))]
    )
  )

   #|(if (negative? n)
      (error "The number must be non-negative")
      (if (zero? n)
          0
          (add1 (count-digits-rec (quotient n 10)))
      )
     )
   |#

(= (count-digits-iter 12345) 5)
(= (count-digits-iter 123) 3)
(= (count-digits-rec 12345) 5)
(= (count-digits-rec 123) 3)



