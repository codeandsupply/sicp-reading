#lang planet neil/sicp

;; Exercise 2.28. Write a procedure fringe that takes as argument a
;; tree (represented as a list) and returns a list whose elements are
;; all the leaves of the tree arranged in left-to-right order. For
;; example,
;;
;;     (define x (list (list 1 2) (list 3 4)))
;;     (fringe x)
;;     (1 2 3 4)
;;     (fringe (list x x))
;;     (1 2 3 4 1 2 3 4)

;; (define (fringe tree)
;;  (cond ((pair? tree) (fringe)))

(define (fringe tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else
         (append (fringe (car tree)) (fringe (cdr tree))))))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))

;; To solve this, I had to check my edge cases carefully. Consider:
;;
;;     (append (list 1 2) nil)
;;     (append (list 1 2)
;;     (append (list 1 2) 1)
;;     (append (list 1 2) (list 1))
;;     (append (list 1 2 3) '() (list 1 2 3) '())
