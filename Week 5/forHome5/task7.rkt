#lang racket
(require racket/trace)

; Take 2 strings s1 and s2 including only letters from a to z. Return a new sorted
; string, the longest possible, containing distinct letters - each taken only once
; - coming from s1 or s2.

(define (longest xs xy)
  (list->string (remove-duplicates (sort (string->list (string-append xs xy)) char<?)))
  )

; Test cases
(equal? (longest "xyaabbbccccdefww" "xxxxyyyyabklmopq") "abcdefklmopqwxy")
(equal? (longest "abcdefghijklmnopqrstuvwxyz" "abcdefghijklmnopqrstuvwxyz") "abcdefghijklmnopqrstuvwxyz")
