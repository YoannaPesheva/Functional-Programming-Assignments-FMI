#lang racket
(require racket/trace)

; Define a procedure that concatenates two lists.

; predefined procedure
(define (concat-proc xs xy)
  (append xs xy)
  )

; linearly iterative process
(define (concat-rec xs xy)
  (define (helper copy-xs copy-xy new-list)
    (cond
      ((null? copy-xs)
       (if (null? copy-xy)
           (reverse new-list)
           (helper copy-xs (cdr copy-xy) (cons (car copy-xy) new-list))
       )
       )
      (else (helper (cdr copy-xs) xy (cons (car copy-xs) new-list)))
    )
  )
   (helper xs xy '())
  )

; using a predefined procedure
(equal? (concat-proc '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

; using a linearly iterative process
(equal? (concat-rec '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))