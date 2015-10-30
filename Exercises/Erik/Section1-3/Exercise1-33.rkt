#lang planet neil/sicp

;; Exercise 1.33. You can obtain an even more general version of
;; accumulate (exercise 1.32) by introducing the notion of a filter on
;; the terms to be combined. That is, combine only those terms derived
;; from values in the range that satisfy a specified condition. The
;; resulting filtered-accumulate abstraction takes the same arguments
;; as accumulate, together with an additional predicate of one
;; argument that specifies the filter. Write filtered-accumulate as a
;; procedure. Show how to express the following using
;; filtered-accumulate:
;;
;; a. the sum of the squares of the prime numbers in the interval a to
;; b (assuming that you have a prime? predicate already written)
;;
;; b. the product of all the positive integers less than n that are
;; relatively prime to n (i.e., all positive integers i < n such that
;; GCD (i,n) = 1).

(define (accumulate-filter combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (combiner
       (if (filter a) (term a) null-value)
       (accumulate-filter combiner null-value term (next a) next b filter))))

(define (square x) (* x x))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (and (> n 1) (= (smallest-divisor n) n)))

;; a
(define (sum-square-prime a b)
  (accumulate-filter + 0 square a inc b prime?))

;; b
(define (relative-prime-product n)
  (define (id x) x)
  (define (gcd a b) (if (= b 0) a (gcd b (remainder a b))))
  (define (relative-prime? x) (= 1 (gcd x n)))
  (accumulate-filter * 1 id 1 inc n relative-prime?))

