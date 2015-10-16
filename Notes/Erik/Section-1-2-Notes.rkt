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
(define (factorial2 n)
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


;; Tree Recursion

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

; The above definition will take a long time for large numbers. Try
; (fib 40). This is because we duplicate work as we decend into the
; tree. The run time is exponential.

; We can solve this:

(define (fib2 n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
; Think of `a` as "last" and `b` as accumulator (acc)


;; Counting Change

;; What is the number of ways to make change for a given amount of
;; money?
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount 
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
