#lang racket

; Task 1
; higher procedure that simulates the 'identity' one;

#|(define (my-identity-n x) ; normal one
  x
  )
|#

(define (my-identity) ; using lambda
  (λ (x) x)
  )

; accepts a procedure and returns a lambda that accepts an argument and applies
; the procedure to it

(define (f proc)
  (λ (x) (proc x))
  )


; accepts a predicate and returns a lambda that accepts an argument and
; applies the negated predicate to it

(define (g p?)
  (λ (x) (not (p? x)))
  )

; returns a procedure that is the partial application of f over x.

(define (my-partially-apply f) ; ???
  (λ (x y) ((curry f) x y) )
  )

; accepts two procedures and return their composition over an argument of a lambda
; procedure

(define (my-compose f g)
  (λ (x) (f (g x))) ; composition
  )

; Task 2
; Define a higher order procedure that accepts
; an unary procedure - f,
; and returns a lambda of two real numbers - x, y,
; that subtracts f(x) from f(y).

(define (difference f)
  (λ (x y) (- (f y) (f x)))
  )

; Task 3
; Define a procedure that returns
; the first order derivative of an unary procedure "f" with precision "eps".

(define (derive f eps)
  (λ (x) (/ (- (f (+ x eps)) (f x)) eps))
  )

; Task 4
; Define a procedure "switch-sum f g n" ("f", "g" - procedures, "n" - number)
; that returns an unary procedure that for every "x" returns
; this sum: f(x) + g(f(x)) + f(g(f(x))) +  ... (containing n elements).

(define (switch-sum f g n)
  (λ (x) (if (zero? n)
             0
             (+ (f x) ((switch-sum g f (sub1 n)) (f x)))
             ))
  )

; Task 5
; Define a higher order procedure repeater str
; that accepts a string and returns
; a linearly recursive procedure of
; two arguments - count (number) and glue (string).
; The result from a call to the result of repeater
; should be a string that is str repeated count times
; with glue being put between every two str instances.

(define (repeater str)
  (λ (count glue)
  (if (= count 1)
      str
      (string-append str glue ((repeater str) (sub1 count) glue))
      )
  )
)



