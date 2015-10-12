#lang planet neil/sicp

;; Exercise 1.10. The following procedure computes a mathematical
;; function called Ackermann’s function.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; What are the values of the following expressions?

(A 1 10)
;; 1024

(A 2 4)
;;65536

(A 3 3)
;; 65536

;; Consider the following procedures, where A is the procedure defined
;; above:

;; Give concise mathematical definitions for the functions computed by
;; the procedures f, g, and h for positive integer values of n. For
;; example, (k n) computes 5n^2.

(define (f n) (A 0 n))
;; f(n) = 2n

(define (g n) (A 1 n))
;; g(n) = 2 ^ n

(define (h n) (A 2 n))
;; h(0) = 1
;; h(n) = 2^h(n-1)

(define (k n) (* 5 n n))
;; k(n) = 5(n^2)


;; Expansion (To help me figure out (A 2 n)
(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
;; ...
(A 1 16)
(A 0 (A 1 15))
;; ...
(A 0 32768)
65536
