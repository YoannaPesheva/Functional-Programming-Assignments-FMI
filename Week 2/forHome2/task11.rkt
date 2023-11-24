#lang racket
(require math/number-theory)
#|
A cubic prime number is a prime number that is the difference between two
subsequent natural numbers. For example, 61 is such a number because
61 = 53 - 43. Define a procedure that returns the nth cubic prime number.
|#

(define (cube n)
  (expt n 3)
  )

(define (nth-cubic n)
  (define (helper copy-n iter counter res)
    (cond
      [(= copy-n counter) res]
      [(prime? (- (cube (add1 iter )) (cube iter))) (helper copy-n (add1 iter) (add1 counter) (- (cube (add1 iter)) (cube iter)))]
      [else (helper copy-n (add1 iter) counter res)]
     )   
   )
   (if (< n 1)
      (error "n was not natural number")
      (helper n 1 0 0)
   )
)

(= (nth-cubic 1) 7)
(= (nth-cubic 4) 61) ; 61 is the 4th cubic prime number
(= (nth-cubic 50) 55897) ; 55897 is the 50th cubic prime number
(= (nth-cubic 100) 283669)
(= (nth-cubic 200) 1570357)
; (nth-cubic 0) ; should return an error
