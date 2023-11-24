#lang racket
(require racket/trace)

#|
 The idea is to check every two numbers, find the smaller one and add it to the sum, then take the list without the number/numbers chosen(with a bit of a twist, because sometimes even though the next stone has smaller risk,
 the risk would be lower if we go to the stone after the next one)
 For example: 401 273 823 715 - here, when we check 273 and 823, we see that 273 < 823, but if we step on 273, then we will either have to step on 823 or 715, so the sum will
 be 273 + 715 = 985, however, if we just stepped on the stone after the next stone - 823, we would lower the risk by 985-823 = 162.
|#

(define (cross-water-min-risk risks)
  (define (helper copy-risk sum)
    (cond
      ((null? (cdr copy-risk)) sum) ; base case - there is only the 'next stone' left so Clyde can just cross the river
      ((null? (cddr copy-risk)) (+ sum (min (car copy-risk) (cadr copy-risk)))) ; base case - there are two stones left and we need to find on which one Clyde needs to step before the program ends
      ((and (= sum 0) (<= (car copy-risk) (cadr copy-risk)) (not (null? (cdddr copy-risk)))) (helper (cdr copy-risk) (+ sum (car copy-risk)))) ; this is for the case in which the idea
      ; I explained would not work, because it is the first step Clyde is ever making
      ((<= (car copy-risk) (cadr copy-risk)) ; using car and cadr, because i am essentially 'taking out' the stone he is currently on too during the recursion, which means car(copy-risk) will be the 'next stone'
       (if (or
            (and
             (< (cadr copy-risk) (+ (car copy-risk) (caddr copy-risk))) ; this is the one step idea
             (< (caddr copy-risk) (cadr copy-risk)))
            (and
             (< (cadr copy-risk) (caddr copy-risk))
             (< (cadr copy-risk) (+ (car copy-risk) (cadr copy-risk)))) ; this is an addition, because: 413 20 210 963 - here we will step on 20 and then 210, because 210 < 963, but from
            ; 413 we dont have to do the step with 20 when we can jump straight to 210 and lower the risk by 20.
            )
           (helper (cddr copy-risk) (+ sum (cadr copy-risk))) ; if returns true
           (helper (cdr copy-risk) (+ sum (car copy-risk))) ; if returns false
           )
       )
      (else (helper (cddr copy-risk) (+ sum (cadr copy-risk)))) ; this else is for cond 
      )
    )
  ; (trace helper)
  (helper risks 0)
  )


(= (cross-water-min-risk '(10 15 20)) 15)
(= (cross-water-min-risk '(1 100 1 1 1 100 1 1 100 1)) 6)
(= (cross-water-min-risk '(458 896 809 929 430 241)) 1697)
(= (cross-water-min-risk '(945 726 301 716 642 149)) 1591)
(= (cross-water-min-risk '(192 31 533 573 772 31)) 635)
(= (cross-water-min-risk '(734 401 273 823 715 216 960 474 91 568)) 2005)
(= (cross-water-min-risk '(793 413 20 210 963 733 992 500 660 43)) 1899)


