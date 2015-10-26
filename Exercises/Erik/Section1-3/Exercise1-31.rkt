#lang planet neil/sicp

;; Exercise 1.31.
;;
;; a. The sum procedure is only the simplest of a vast number of
;; similar abstractions that can be captured as higher-order
;; procedures. 51 Write an analogous procedure called product that
;; returns the product of the values of a function at points over a
;; given range. Show how to define factorial in terms of product. Also
;; use product to compute approximations to using the formula 52 
;;
;; b. If your product procedure generates a recursive process, write
;; one that generates an iterative process. If it generates an
;; iterative process, write one that generates a recursive process.


;; a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

;; b
(define (product-b term a next b)
  (define (product-iter term a next b acc)
    (if (> a b)
        acc
        (product-iter term (next a) next b (* acc (term a)))))
  (product-iter term a next b 1))
