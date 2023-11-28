#lang racket

; Define a procedure that removes an element from a list.

; no predefined procedure
(define (remove-all-no-proc x xs)
  (cond
    [(null? xs) xs]
    [(equal? x (car xs)) (remove-all-no-proc x (cdr xs))]
    [else (cons (car xs) (remove-all-no-proc x (cdr xs)))]
    )
  )

; using remove*
(define (remove-all-proc x xs)
  (remove* (list x) xs)
  )

; Test cases

; without using a predefined procedure
(equal? (remove-all-no-proc 1 '(1 1 1 2)) '(2))
(equal? (remove-all-no-proc 1 '(2 5 6)) '(2 5 6))
(equal? (remove-all-no-proc 1 '(1)) '())
(equal? (remove-all-no-proc 1 '(1 2 1 1)) '(2))
(equal? (remove-all-no-proc "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN"))

; using a predefined procedure
(equal? (remove-all-proc 1 '(1 1 1 2)) '(2))
(equal? (remove-all-proc 1 '(2 5 6)) '(2 5 6))
(equal? (remove-all-proc 1 '(1)) '())
(equal? (remove-all-proc 1 '(1 2 1 1)) '(2))
(equal? (remove-all-proc "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN"))