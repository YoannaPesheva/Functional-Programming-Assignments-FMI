#lang racket
(require racket/trace)
(require math/number-theory) ; for the factorial in the last task
; Task 1
#|
A narcissistic number is a number which is the sum of its own
digits, each raised to the power of the number of digits in a
given base. Define a predicate that checks whether a given
number is a narcissistic number.
|#

(define (num-digits n) ; finds how many digits there are in a number
  (if (< n 10)
      1
      (add1 (num-digits (quotient n 10)))
      )
  ) ; we need it for the power

(define (narcissistic? n)
  (define (narcissistic-sum leftover power)
    (if (zero? leftover)
        0
        (+
         (expt (remainder leftover 10) power)
         (narcissistic-sum (quotient leftover 10) power)
         )
        )
    )
  (= n (narcissistic-sum n (num-digits n)))
  )

(equal? (narcissistic? 7) #t)
(equal? (narcissistic? 12) #f)
(equal? (narcissistic? 370) #t)
(equal? (narcissistic? 371) #t)
(equal? (narcissistic? 1634) #t)

; Task 2
#|
Define an iterative procedure that accepts two numbers x and n and calculates
the following sum: 1 + x + x^2 + x^3 + ... + x^n.
|#

(define (calculate-sum x n)
  (define (helper result power)
    (if (zero? power)
        result
        (helper (+ result (expt x power)) (sub1 power))     
        )
    )
  (helper 1 n)
  )

(= (calculate-sum 5 0) 1)
(= (calculate-sum 5 1) 6)
(= (calculate-sum 10 1) 11)
(= (calculate-sum 1 11) 12)
(= (calculate-sum 2 11) 4095)

; Task 3
#|
Define a procedure that finds the maximum digit in a number.
|#

(define (find-max n)
  (define (helper leftover curr-max)
    (if (zero? leftover)
        curr-max
        (helper (quotient leftover 10) (max curr-max (remainder leftover 10)))
     )
   )
  ; (trace helper)
  (helper (quotient n 10) (remainder n 10))
  
)

(= (find-max 55345) 5)
(= (find-max 14752) 7)
(= (find-max 329450) 9)
(= (find-max 9521) 9)

; Task 4
#|
Define a recursive procedure (sum-numbers start finish) that returns the sum of
all numbers from the interval [start, finish] whose digits are ordered
in decreasing order.
|#

(define (decreasing? n)
  (or (< n 10)
      (and
       (<= (remainder n 10) (remainder (quotient n 10) 10))
       (decreasing? (quotient n 10))
       )
      )
  )

(define (sum-numbers start finish)
  (define (helper true-start true-finish)
    (cond
      [(> true-start true-finish) 0]
      [(decreasing? true-start) (+ true-start (helper (add1 true-start) true-finish))]
      [else (helper (add1 true-start) true-finish)]
      )
   )
  (helper (min start finish) (max start finish))
 )

(= (sum-numbers 1 9) 45)
(= (sum-numbers 199 203) 200)
(= (sum-numbers 219 225) 663)
(= (sum-numbers 225 219) 663)

; Task 5
#|
Define a predicate that accepts two non-negative inputs - x and y,
and checks whether x is a sub-number of y.
|#

; we have to use num-digits

(define (sub-num? x y)
  (and
   (<= x y)
   (or
    (= x (remainder y (expt 10 (num-digits x))))
    (sub-num? x (quotient y 10))
    )
   )
  )

(equal? (sub-num? 123 5123783) #t)
(equal? (sub-num? 0 0) #t)
(equal? (sub-num? 10 101) #t)
(equal? (sub-num? 101 101) #t)
(equal? (sub-num? 10 0) #f)
(equal? (sub-num? 1253 5123783) #f)
(equal? (sub-num? 12 0) #f)

; Task 6
#|
A digital root is the recursive sum of all the digits in a number. Given n, take the
sum of the digits of n. If that value has more than one digit, continue reducing
in this way until a single-digit number is produced. This is only applicable to
the natural numbers.
|#

(define (sum-digits n)
  (if (< n 10)
      n
      (+ (sum-digits (quotient n 10)) (remainder n 10))
      )
  )

(define (digital-root n)
  (if (< n 10)
      n
      (digital-root (sum-digits n))
      )
  )

(= (digital-root 16) 7)
; => 1 + 6
; => 7
(= (digital-root 942) 6)
; => 9 + 4 + 2
; => 15
; => 1 + 5
; => 6
(= (digital-root 132189) 6)
(= (digital-root 493193) 2)

; Task 7
#|
Define the procedure that accepts
a whole number - n, and a real number (representing radians) - x,
and returns the nth partial sum of sin(x).
|#

(define (my-sin n x)
  (if (zero? n)
      x
      (+
       (/
        (* (expt -1 n) (expt x (add1 (* 2 n))))
        (factorial (add1 (* 2 n)))
        
        )
       (my-sin (sub1 n) x)
       )
      )
  )

(= (my-sin 100 1.570796) 0.9999999999999465) ; 90 degrees => 0.9999999999999465
(= (my-sin 100 0.5235988) 0.5000000211324931) ; 30 degrees => 0.5000000211324931

























