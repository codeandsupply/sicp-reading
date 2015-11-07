#lang planet neil/sicp

;; Define a procedure last-pair that returns the list that contains
;; only the last element of a given (nonempty) list:

;;     (last-pair (list 23 72 149 34))
;;     (34)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(last-pair (list 23 72 149 34))
