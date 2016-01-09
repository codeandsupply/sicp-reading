;; Exercise 2.61. Give an implementation of adjoin-set using the
;; ordered representation. By analogy with element-of-set? show how to
;; take advantage of the ordering to produce a procedure that requires
;; on the average about half as many steps as with the unordered
;; representation.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
        ((= x (car set)) set)
        ((> x (car set))
         (cons (car set) (adjoin-set x (cdr set))))
        (else (cons x set))))

(adjoin-set 3 (list 1 2 3 4 5))
(adjoin-set 6 (list 1 2 3 4 5))
(adjoin-set 3 (list 1 2 4 5))

;; Assuming elements are placed into the set at random intervals, this
;; will take on average, n/2 operations.
