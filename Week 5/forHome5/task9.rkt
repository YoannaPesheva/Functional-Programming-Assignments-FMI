#lang racket

; Define a procedure (insert-at x i xs) that inserts an element at a given index
; in a list.

(define (insert-at x i xs)
  (define (helper first-half second-half)
    (append first-half (cons x second-half))
    )
  (if (or (negative? i) (> i (length xs)))
      (error "Invalid index")
      (helper (take xs i) (drop xs i))
      )
  )

; Test cases
(equal? (insert-at 1 0 '()) '(1))
(equal? (insert-at 1 0 '(2)) '(1 2))
(equal? (insert-at 10 1 '(1 2 3)) '(1 10 2 3))
(equal? (insert-at 1 0 '()) '(1))
(equal? (insert-at 1 0 '(2)) '(1 2))
(equal? (insert-at 10 1 '(1 2 3)) '(1 10 2 3))
(equal? (insert-at 7 0 '(1 2 3)) '(7 1 2 3))
(equal? (insert-at 7 1 '(1 2 3)) '(1 7 2 3))
(equal? (insert-at 7 3 '(1 2 3)) '(1 2 3 7))
(insert-at 7 4 '(1 2 3)) ; error: Invalid index!