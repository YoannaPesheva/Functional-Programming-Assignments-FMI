#lang racket

;Task 1
(define (my-min-built-in-p x y)
  (min x y)
  )

(define (my-min-if x y)
  (if(< x y)
     x
     y
     )
  )

(define (my-min-guard x y)
  (cond
    [(< x y) x]
    [else y]
    )
 )

(define (last-digit x)
  (remainder x 10)
 )

(define (quotient-whole x y)
  (quotient x  y)
  )

(define (div-whole x y)
  (/ x y)
  )

(define (remove-last-digit x)
  (quotient x 10)
  )

(define (div-real x y)
  (/ (* x 1.0) y)
  )