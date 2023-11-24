#lang racket
#|
Define a predicate that accepts a natural number n and returns whether n2
ends in the digits of n.
|#

(define (automorphic? n)
  (define (helper copy-n n-squared)
    (cond
      [(zero? copy-n)]
      [(= (remainder copy-n 10) (remainder n-squared 10)) (helper (quotient copy-n 10) (quotient n-squared 10))]
      [else #f]
      )
    )
  (if (< n 1)
     (error "n was not a natural number.")
     (helper n (* n n))
  )
)

(equal? (automorphic? 3)#f)
(equal? (automorphic? 10)#f)
(equal? (automorphic? 5)#t)
(equal? (automorphic? 25)#t)
(equal? (automorphic? 76)#t) 
(equal? (automorphic? 890625)#t) 
(equal? (automorphic? 625)#t) 
(equal? (automorphic? 36) #f)
(equal? (automorphic? 11) #f)
; (automorphic? -1) ; error: n was not natural
; (automorphic? 0) ; error: n was not natural

