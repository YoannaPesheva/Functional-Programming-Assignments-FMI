#lang racket

(define (p n)
  (define (helper n sum)
    (cond
      [(= n 1) sum]
      [else (helper (sub1 n) (+ sum (- (* 3 n) 2)))]
      )
    )  
  (if (not (positive? n))
      (error "The number must be non-negative")
      (helper n 1)
      )
  )


; Tests
(= (p 1) 1)
(= (p 2) 5)
(= (p 3) 12)
(= (p 4) 22)
(= (p 5) 35)
(= (p 6) 51)