#lang planet neil/sicp

;; Exercise 1.16. Design a procedure that evolves an iterative
;; exponentiation process that uses successive squaring and uses a
;; logarithmic number of steps, as does fast-expt. (Hint: Using the
;; observation that (b^n/2)^2 = (b^2)^n/2 , keep, along with the
;; exponent n and the base b, an additional state variable a, and
;; define the state transformation in such a way that the product
;; a*b^n is unchanged from state to state. At the beginning of the
;; process a is taken to be 1, and the answer is given by the value of
;; a at the end of the process. In general, the technique of defining
;; an invariant quantity that remains unchanged from state to state is
;; a powerful way to think about the design of iterative algorithms.)

;; Utilities
(define (even? n)
  (= (remainder n 2) 0))
(define (square x) (* x x))

;; O(log(n)) time, O(log(n)) space
(define (fast-expt-recursive b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt-recursive b (/ n 2))))
        (else (* b (fast-expt-recursive b (- n 1))))))

;; Here we use the invariant a*b^n
(define (fast-expt b n)
  (fast-expt-iter b n 1))
(define (fast-expt-iter b n a)
  (if (= n 0)
      a
      (fast-expt-iter 
       (if (even? n) (square b) b)
       (if (even? n) (/ n 2) (- n 1))
       (if (even? n) a (* a b)))))
