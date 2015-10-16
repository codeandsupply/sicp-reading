#lang planet neil/sicp

;; Exercise 1.11 A function f is defined by the rule that f(n) = n if
;; n < 3 and f(n) = f(n - 1) + 2*f(n - 2) + 3*f(n - 3) if n>=3. Write
;; a procedure that computes f by means of an interative process

;; Answer
;;;;;;;;;

;; Naive, bad implementation
(define (treef n)
  (cond ((< n 3) n)
        ((>= n 3)
         (+
          (treef (- n 1))
          (* 2 (treef (- n 2)))
          (* 3  (treef (- n 3)))))))

;; Linear, good implementation

(define (f n)
  (if (< n 3)
      n
      (f-iter 2 1 0 n)))

(define (f-iter back1 back2 back3 count)
  (if (< count 3)
      back1
      (f-iter (+ back1 (* 2 back2) (* 3 back3))
              back1
              back2
              (- count 1))))
