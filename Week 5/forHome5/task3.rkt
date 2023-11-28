#lang racket

; Define procedures that accept a list of digits and return the number that is build
; by traversing the list from right to left. Create two versions - one that utilizes
; folding, and another that does recursion.

; foldr procedure
(define (rev-fold xs)
  (foldr (λ (x acc) (+ x (* acc 10))) 0 xs)
  )

(define (rev-lin-iter xs)
  (define (helper copy-xs acc)
    (if (null? copy-xs)
        acc
        (helper (cdr copy-xs) (+ (car copy-xs) (* acc 10)))
        )
    )
  (helper (reverse xs) 0)
  )

; Test cases

; using a linearly iterative procedure
(= (rev-lin-iter '(1 2 3)) 321)
(= (rev-lin-iter '(1 2 3 4 5 6 7 8 9)) 987654321)

; using folding
(= (rev-fold '(1 2 3)) 321)
(= (rev-fold '(1 2 3 4 5 6 7 8 9)) 987654321)