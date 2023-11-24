#lang racket

(define (apply-n f n)
  (define (helper x cnt)
    (if (zero? cnt)
        x
        (helper (f x) (sub1 cnt))
        )
    )
  (λ (x) (helper x n))
  )

(= ((apply-n (λ (x) (* 2 x)) 5) 2) 64)
(= ((apply-n (λ (x) (quotient x 10)) 2) 100) 1)
