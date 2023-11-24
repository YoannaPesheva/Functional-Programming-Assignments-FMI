#lang racket

; using the procedure from the in-class tasks
(define (derive f eps)
  (λ (x)
    (/ (- (f (+ x eps)) (f x)) eps)))

(define (derive-n f n eps)
  (if (zero? n)
      f
      (derive-n (λ (x) ((derive f eps) x)) (sub1 n) eps)
      )
  )

(= ((derive-n (λ (x) (* 2 x x x)) 3 1e-3) 2) 12.000015203739167)
; ((derive-n (λ (x) (* 2 x x x)) 3 1e-3) 2)