#lang racket

; Task 1
(define (bouncing-ball height bounce window)
  (cond
    [(>= window height) -1]
    [(< height window) 0]
    [(> (* height bounce) window)
     (+ (bouncing-ball (* height bounce) bounce window) 2) ; we add 2 because it goes up
                                                           ; and down
    ]
    [else 1] ; in case the bounce does not reach the window,
    ; we end the recursion but add 1, because height > window, so she will see
    ; the ball at least once in all cases
  )
)

; Task 2
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
       (helper copy-year (add1 iter) (+ sum (floor (* (expt 0.8 (- copy-year iter)) 300 3)) )) ; this is assuming he did plant 3 trees the first year too (which is the way the tests
                                                                                               ; are right
      ]
     )
  )
 (helper year 1 0)
)

; Task 1 tests
(= (bouncing-ball 3 0.66 1.5) 3)
(= (bouncing-ball 30 0.66 1.5) 15)
(= (bouncing-ball 10 0.6 10) -1)
(= (bouncing-ball 2 0.5 1.0) 1)

; Task 2 tests
; they slightly differ, I believe it is because of the rounding
(num-apples 2 1) ; 900
(num-apples 4 8) ; 2655
(num-apples 74 10) ; 3984
(num-apples 1 15) ; 900
(num-apples 52 77) ; 4377