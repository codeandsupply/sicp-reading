;; Exercise 4.29.  Exhibit a program that you would expect to run much
;; more slowly without memoization than with memoization. Also,
;; consider the following interaction, where the id procedure is
;; defined as in exercise 4.27 and count starts at 0:

(define (square x)
  (* x x))

;;; L-Eval input:
; (square (id 10))

;;; L-Eval value:
; <response>          mem: 100  nomem: 100

;;; L-Eval input:
; count

;;; L-Eval value:
; <response>          mem: 1  nomem: 2


;; Give the responses both when the evaluator memoizes and when it does not.
