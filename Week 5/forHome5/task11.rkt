#lang racket

; By using recursion define a procedure that implements the list-ref
; procedure we discussed in class.

(define (my-list-ref xs i)
  (cond
    ((or (negative? i) (> i (length xs))) (error "Invalid index!"))
    ((zero? i) (car xs))
    (else (my-list-ref (cdr xs) (sub1 i)))
    )
  )

; Test cases
(= (my-list-ref '(1 2 3) 0) 1)
(= (my-list-ref '(1 2 3) 1) 2)
(equal? (my-list-ref '("Hello" 2 ("nested list")) 0) "Hello")
(my-list-ref '(1 2 3) -100) ; error: Invalid index!