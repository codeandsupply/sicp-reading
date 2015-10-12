#lang planet neil/sicp

;; Factorials

; Obvious definition
;
; A linear-recursive process
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

; Iteration version
;
; A linear-iterative process
(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

; Recursive Process vs Procedure
;
; Recursive Procedure
;  - A procedure that refers to itself syntactically
; Recursive Process
;  - The process evolution evolves recursively (say, storing data on the stack)

; Tail recursion
;
;  - An interpreter that implements tail-recursion can run iterative
;  processes in constant space


