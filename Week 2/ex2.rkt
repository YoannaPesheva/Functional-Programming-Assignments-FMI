#lang racket

; Task 1
#|
Define predicates that check:
whether two numbers are not equal:
 *in one line without using if-else;
 *using guards.
whether a whole number x is between
two whole numbers - start and end:
 *in one line without using if-else;
 *with a local helper procedure that uses boolean operators.
|#

; btw predicates is just the fancy word for functions
; in functional programming


(define (not-equal-one-line? x y)
(not (= x y))
  )

(define (not-equal-guards? x y)
  (cond
    [(= x y) #f]
    [else #t]
  )
 )

(define (inside-one-line? start end x)
  (<= (min start end) x (max start end))
  )

(define (inside-boolean-ops? start end x)
  (and
   (<= (min start end) x)
   (<= x (max start end))
   )
  )

(equal? (not-equal-one-line? 5 2) #t)
(equal? (not-equal-one-line? 5 5) #f)

(equal? (not-equal-guards? 5 2) #t)
(equal? (not-equal-guards? 5 5) #f)

(equal? (inside-one-line? 1 5 4) #t) ; start = 1, end = 5, x = 4
(equal? (inside-one-line? 5 1 4) #t)
(equal? (inside-one-line? 10 50 200) #f)
(equal? (inside-one-line? 10 50 1) #f)

(equal? (inside-boolean-ops? 1 5 4) #t)
(equal? (inside-boolean-ops? 5 1 4) #t)
(equal? (inside-boolean-ops? 10 50 200) #f)
(equal? (inside-boolean-ops? 10 50 1) #f)

; Task 2
#|
Define two procedures that return whether a number is even:
- using if-else;
- using guards.
|#

(define (even-if? n)
  (if (zero? (remainder n 2)) "Yes" "No")
 )

(define (even-guards? n)
  (cond
    [(zero? (remainder n 2)) "Yes"]
    [else "No"]
    )
  )

(equal? (even-if? 2) "Yes")
(equal? (even-if? 15452) "Yes")
(equal? (even-if? 321) "No")

(equal? (even-guards? 2) "Yes")
(equal? (even-guards? 15452) "Yes")
(equal? (even-guards? 321) "No")

; Task 3
#|
A number is a palindrome if and only if it is the same
number from right to left as well as from left to right.
Define a predicate that checks whether a non-negative number is a palindrome.
|#

(define (rev x)
(define (helper result leftover)
  (if (zero? leftover)
      result
      (helper (+ (* result 10) (remainder leftover 10)) (quotient leftover 10))
      )
  )

  helper( 0 x)
  )