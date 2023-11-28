#lang racket

; Define a procedure that reverses a list using foldl.

(define (my-reverse-foldl xs)
  (foldl (λ (x accum) (cons x accum)) '() xs)
  )

; Test cases
(equal? (my-reverse-foldl '(1 2 3 4 5)) '(5 4 3 2 1))
(equal? (my-reverse-foldl '(11 2 3 8 5)) '(5 8 3 2 11))