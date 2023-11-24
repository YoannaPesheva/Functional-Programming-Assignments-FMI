#lang racket
(require racket/trace)

; find the biggest digit in a given number
(define (find-max n)
  (define (helper leftover curr-max)
    (if (zero? leftover)
        curr-max
        (helper (quotient leftover 10) (max curr-max (remainder leftover 10)))
        )
    )
  (helper (quotient n 10) (remainder n 10))
  )

; remove the first occurrence of the digit
(define (remove-first-occurrence num digit)
  (define (helper copy-n result removed? counter)
    (cond
      ((zero? copy-n) result)
      ((= (remainder copy-n 10) digit)
       (if removed?
           (helper (quotient copy-n 10) (+ (* (remainder copy-n 10) (expt 10 counter)) result) #t (add1 counter))
           (helper (quotient copy-n 10) result #t counter)
           )
       )
      (else
       (helper (quotient copy-n 10) (+ (* (remainder copy-n 10) (expt 10 counter)) result) removed? (add1 counter))
       )
      )
    )
  (helper num 0 #f 0)
  )

; procedure to count how many digits a given number has
(define (amount-of-digits n)
  (if (zero? n)
      0
      (add1 (amount-of-digits (quotient n 10)))
      )
  )

(define (sort-n n)
  (define (helper copy-n result digits-left)
    (if (zero? digits-left)
        result
        (helper (remove-first-occurrence copy-n (find-max copy-n)) (+ (* (find-max copy-n) (expt 10 (sub1 digits-left))) result) (sub1 digits-left))  
        )
    )
  ; (trace helper)
  (helper n 0 (amount-of-digits n))
  )

; Tests
(= (sort-n 1714) 7411)
(= (sort-n 123450) 543210)
(= (sort-n 123405) 543210)
(= (sort-n 123045) 543210)
(= (sort-n 120345) 543210)
(= (sort-n 102345) 543210)
(= (sort-n 8910) 9810)
(= (sort-n 321) 321)
(= (sort-n 29210) 92210)
(= (sort-n 1230) 3210)
(= (sort-n 55345) 55543)
(= (sort-n 14752) 75421)
(= (sort-n 329450) 954320)
(= (sort-n 9125) 9521)