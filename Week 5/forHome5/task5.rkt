#lang racket
(require math/number-theory)
(require racket/trace)

; According to the fundamental theorem of arithmentics every natural number
; greater than 2 can be expressed as a product of prime numbers.
; Define a procedure that returns the sorted list of prime factors of a natural number.

(define (find-primes n)
  (define (helper copy-n iter xs)
    (cond
      ((<= copy-n 1) xs)
      ((and (prime? iter) (divides? iter copy-n)) (helper (/ copy-n iter) iter (cons iter xs)))
      (else (helper copy-n (add1 iter) xs))
      )
    )
  ; (trace helper)
  (helper n 2 '())
  )

(define (factorize n)
  (if (< n 2)
      (error "Number should be equal or greater than 2")
      (sort (find-primes n) <)
      )
  )

; Test cases
(equal? (factorize 2) '(2))
(equal? (factorize 6) '(2 3))
(equal? (factorize 13) '(13))
(equal? (factorize 123) '(3 41))
(equal? (factorize 152) '(2 2 2 19))
(equal? (factorize 12356498) '(2 7 11 19 41 103))