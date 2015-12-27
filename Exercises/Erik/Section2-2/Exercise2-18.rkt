#lang planet neil/sicp

;; Exercise 2.18. Define a procedure reverse that takes a list as
;; argument and returns a list of the same elements in reverse order:
;;
;;     (reverse (list 1 4 9 16 25))
;;     (25 16 9 4 1)

(define (reverse x)
  (if (null? (cdr x))
      x
      (cons (reverse (cdr x)) (car x))))

(reverse (list 1 4 9 16 25))

;; - Could do this better with append
;; - Maybe if we did this iteratively, it would work how I expect.
