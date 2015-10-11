;; Exercise 1.7. The good-enough? test used in computing square roots
;; will not be very effective for finding the square roots of very
;; small numbers. Also, in real computers, arithmetic operations are
;; almost always performed with limited precision. This makes our test
;; inadequate for very large numbers. Explain these statements, with
;; examples showing how the test fails for small and large numbers. An
;; alternative strategy for implementing good-enough? is to watch how
;; guess changes from one iteration to the next and to stop when the
;; change is a very small fraction of the guess. Design a square-root
;; procedure that uses this kind of end test. Does this work better
;; for small and large numbers?

;; Explanation
;;;;;;;;;;;;;;

;; Consider trying to find the square root of 0.0000001. Because we
;; stop our procedure when we're within 0.0001 of our guess, we won't
;; be anywhere near the correct answer. For large numbers, we may not
;; even have enough floating point precision to reach the limit we're
;; imposing.


;; This implementation seems to work better for small numbers than for
;; large. I'm not entirely sure why though.

;; Implementation
;;;;;;;;;;;;;;;;;

(define (sqrt-iter guess last-guess x)
  (if (good-enough-change? guess last-guess x)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough-change? guess last-guess x)
  (< (abs (/ (- guess last-guess) x)) 0.000001))

(define (sqrt x) (sqrt-iter 1.0 x x))

