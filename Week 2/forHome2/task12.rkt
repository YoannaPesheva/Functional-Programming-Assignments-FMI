#lang racket

(define (power-of-2 n)
  (define (helper iter sum)
    (cond
      [(= iter n) sum]
      [else (helper (add1 iter) (+ (expt 2 iter) sum))]
      )
    )
  (helper 0 0)
  )

(define (calculate-part x y n)
  (+ x (* y (power-of-2 n)))
  )

(define (find-sum x y n)
  (+ (calculate-part x y n) (calculate-part x y (sub1 n)) (calculate-part x y (- n 2)))
  )

(= (find-sum 0 2 10) 3578) ; 510 + 1022 + 2046
(= (find-sum 5 3 5) 174) ; 26 + 50 + 98
