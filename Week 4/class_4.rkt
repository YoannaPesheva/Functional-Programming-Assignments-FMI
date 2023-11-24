#lang racket

; Task 1
; Define a procedure that returns the value of a given key from an associative list.
(define (assoc-rec key xs)
  (cond
    [(null? xs) #f]
    [(equal? key (caar xs)) (cdar xs)] ; caar == car( car ), up to 5 a's
    ; (cdr (car xs)) for the second element of the first pair
    [else (assoc-rec key (cdr xs))]
    ) 
  )

(define (assoc-hop key xs)
  (let
      ([filtered-res (filter (λ (pair) (equal? (car pair) key)) xs)])
    (if (null? filtered-res)
        #f
        (cdar filtered-res)
        )
    )
  )

(define (assoc-builtin key xs)
  (let
      ([assoc-res (cdr (assoc key xs))])
    (if(pair? assoc-res)
       (cdr assoc-res)
       #f
       )    
    )
  )

; using a recursive process
(equal? (assoc-rec 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) "One")

; using a higher order procedure
 (equal? (assoc-hop 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) "One")
 (equal? (assoc-hop 42 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) #f)

; using a built-in procedure
 (equal? (assoc-builtin 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) "One")


; Task 2
; Define a procedure (replace xs dict) that replaces
; any of the data in the list xs that has corresponding values in the dictionary dict.
(define (replace xs dict)
  (map (λ (x)
         (let
             ([assoc-res (assoc x dict)])
           (if assoc-res
               (cdr assoc-res)
               x
               )
           )
         xs

         )
       )
  
  )

(equal? (replace '(1 2 3 4) '((1 . 11) (2 . 22))) '(11 22 3 4))
