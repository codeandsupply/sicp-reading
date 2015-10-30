#lang planet neil/sicp

;; Exercise 1.44. The idea of smoothing a function is an important
;; concept in signal processing. If f is a function and dx is some
;; small number, then the smoothed version of f is the function whose
;; value at a point x is the average of f(x-dx), f(x), and
;; f(x+dx). Write a procedure smooth that takes as input a procedure
;; that computes f and returns a procedure that computes the smoothed
;; f. It is sometimes valuable to repeatedly smooth a function (that
;; is, smooth the smoothed function, and so on) to obtained the n-fold
;; smoothed function. Show how to generate the n-fold smoothed
;; function of any given function using smooth and repeated from
;; exercise 1.43.

(define (smooth f dx)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

(define (repeated f n)
  (define (repeat f n x)
    (if (= n 0)
        x
        (repeat f (- n 1) (f x))))
  (lambda (x) (repeat f n x)))

(define (n-smooth f dx n)
  (define (smoother func) (smooth f dx))
  ((repeated smoother n) f))
