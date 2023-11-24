#lang racket
(require racket/trace)

(define (remove-first-occurrence num digit)
  (define (helper copy-n result removed? counter)
    (cond
      ((and (zero? copy-n) (not removed?) (error "The digit was not found in the given number"))) ; base case if we go through the whole number and the digit is not found
      ((zero? copy-n) result) ; base case
      ((= (remainder copy-n 10) digit)
       (if removed?
           ; if it is not the first occurrence
           (helper (quotient copy-n 10) (+ (* (remainder copy-n 10) (expt 10 counter)) result) #t (add1 counter))
           ; if it is the first occurrence
           (helper (quotient copy-n 10) result #t counter)
           )
       )
      ; when it is not equal to the digit we are looking for
      (else
       (helper (quotient copy-n 10) (+ (* (remainder copy-n 10) (expt 10 counter)) result) removed? (add1 counter))
       )
      )
    )
  ; (trace helper)
  (helper num 0 #f 0)
  )

; Tests
(= (remove-first-occurrence 15365 5) 1536)
(= (remove-first-occurrence 15360 0) 1536)
(= (remove-first-occurrence 15300 0) 1530)
(= (remove-first-occurrence 15365 1) 5365)
(= (remove-first-occurrence 35365 3) 3565)
(= (remove-first-occurrence 1212 1) 122)
(= (remove-first-occurrence 1212 2) 121)
(= (remove-first-occurrence (remove-first-occurrence 1212 1) 1) 22)
