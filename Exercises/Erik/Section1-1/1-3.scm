;; Exercise 1.3. Define a procedure that takes three numbers as
;; arguments and returns the sum of the squares of the two larger
;; numbers.

(define (square x) (* x x))
(define (sos x y) (+ (square x) (square y)))

(define (sum-of-larger-squares x y z)
  (cond ((and (> x z) (> y z)) (sos x y))
        ((and (> x y) (> z y)) (sos x z))
        ((and (> y x) (> z x)) (sos y z))))
