#lang racket
#|
Define a recursive and an iterative procedure that returns the number of
palindromes in the interval [a, b]
|#

(define (rev n)
  (define (helper copy-n res)
    (if (zero? copy-n)
        res
        (helper (quotient copy-n 10) (+ (remainder copy-n 10) (* res 10)))
        )
    )
  (helper n 0)
  )

(define (palindrome? n)
  (= n (rev n))
  )

; Iterative
(define (num-palindromes-iter x y)
  (define (helper start counter end)
    (cond
      [ (> start end) counter]
      [(palindrome? start) (helper (add1 start) (add1 counter) end)]
      [else (helper (add1 start) counter end)]
      )
    )
  (helper (min x y) 0 (max x y))
  )

;Recursive
(define (num-palindromes-rec x y)
  (define (helper start end)
    (cond
      [(> start end) 0]
      [(palindrome? start) (add1 (helper (add1 start) end))]
      [else (helper (add1 start) end)]
      )
    )
  (helper (min x y) (max x y))
  )

(= (num-palindromes-iter 1 101) 19)
(= (num-palindromes-iter 1 100) 18)
(= (num-palindromes-iter 100 1) 18)
(= (num-palindromes-rec 1 101) 19)
(= (num-palindromes-rec 1 100) 18)
(= (num-palindromes-rec 100 1) 18)

