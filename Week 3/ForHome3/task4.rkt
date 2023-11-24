#lang racket
(require math/number-theory)
(require racket/trace)

(define (sum-digits n)
  (define (helper copy-n sum)
    (if (zero? copy-n)
        sum
        (helper (quotient copy-n 10) (+ sum (remainder copy-n 10)))
        )
    )
  (helper n 0)
  )

(define (sum-divisible-numbers start finish k)
  (define (helper curr-start end)
    (cond
      [(> curr-start end) 0]
      [(divides? k (sum-digits curr-start)) (+ curr-start (helper (add1 curr-start) end))]
      [else (helper (add1 curr-start) end)]
      )
    )
  ; (trace helper)
  (helper (min start finish) (max start finish))
  )

; Tests
(= (sum-divisible-numbers 0 10 5) 5)
(= (sum-divisible-numbers 0 100 5) 990)
(= (sum-divisible-numbers 100 0 5) 990)