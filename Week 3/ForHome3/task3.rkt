#lang racket

(require math/number-theory)
(require racket/trace)

; multiplying the odd numbers
(define (multiply-odds n)
  (define (helper result count iter)
    (if (= count n)
        result
        (helper (* result iter) (add1 count) (+ iter 2))
        )
    )
  (helper 1 0 1)
  )

; calculating one part of the sequence
(define (calc-one-sum x n)
  (*
   (/
    (expt x n) (multiply-odds n))
   (expt -2 n))
  )

(define (calc-series-sum x n)
  (define (helper sum iter counter)
    (if (= counter n)
        sum
        (helper (+ sum (calc-one-sum x iter)) (add1 iter) (add1 counter))
        )
    )
  ; (trace helper)
  (helper -2 2 0)
  )

; Tests
(calc-series-sum 1 0) ; -2
(calc-series-sum 1 1) ; -2/3
(calc-series-sum 1 2) ; -1 1/5
(calc-series-sum 1 3) ; -1 1/21
(calc-series-sum 1 4) ; -1 11/135
(calc-series-sum 1 5) ; -1 29/385
(calc-series-sum 1 6) ; -1 937/12285