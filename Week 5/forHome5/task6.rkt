#lang racket

; Define a procedure that accepts a list of numbers and returns an unary procedure
; of a natural number - k, such that the result from a call to it (the new procedure)
; is the kth largest negative number in the list.

(define (list-of-negatives xs)
  (remove-duplicates (sort (filter negative? xs) >))
  )

(define (kth-max-min xs)
  (λ (k)
    (if (or (< k 0) (> k (length (list-of-negatives xs))))
        (error "No such number!")
        (list-ref (list-of-negatives xs) (sub1 k))
        )
    )
  )

; Test cases
(= ((kth-max-min '(-1)) 1) -1)
(= ((kth-max-min '(-1 -5 -6 -6 -6 -6)) 2) -5)
(= ((kth-max-min '(-1 -1 -5 -6 -6 -6 -6)) 2) -5)
(= ((kth-max-min '(1 2 3 4 -5 6 7 -2 -1 0)) 2) -2)
((kth-max-min '(-1 0 -1 0 -2 3 1 -1)) 3) ; error: No such number!
