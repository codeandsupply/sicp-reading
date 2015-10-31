#lang planet neil/sicp

;; Exercise 2.1. Define a better version of make-rat that handles both
;; positive and negative arguments. Make-rat should normalize the sign
;; so that if the rational number is positive, both the numerator and
;; denominator are positive, and if the rational number is negative,
;; only the numerator is negative.

(define (xor x y) (and (or x y) (not (and x y))))

(define (make-rat n d)
  (let ((g (gcd n d))
        (num (abs n))
        (den (abs d)))
    (if (xor (< 0 n) (< 0 d))
        (cons (/ (- num) g) (/ den g))
        (cons (/ num g) (/ den g)))))
