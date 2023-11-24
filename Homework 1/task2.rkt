#lang racket

(define (num-apples year span)
  (if (or (<= year 0) (<= span 0))
      (error "You must enter positive numbers.")
      0
   )
   
  (define (helper copy-year iter sum)
    (cond
      [(negative? (- copy-year iter)) ; the end of the recursion
       sum
      ]
      [(>= (- copy-year iter) span) ; if they are cut off and will not produce any apples in the year
       (helper copy-year (add1 iter) sum)
      ]
      [else ; if the trees are not cut off and will produce apples in the year
       (helper copy-year (add1 iter) (+ sum (floor (* (expt 0.8 (- copy-year iter)) 300 3)) ))
      ]
     )
  )
 (helper year 1 0)
)

(num-apples 2 1)
(num-apples 4 8)
(num-apples 74 10)
(num-apples 1 15)
(num-apples 52 77)