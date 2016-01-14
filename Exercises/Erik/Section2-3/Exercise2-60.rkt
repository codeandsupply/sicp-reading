;; Exercise 2.60. We specified that a set would be represented as a
;; list with no duplicates. Now suppose we allow duplicates. For
;; instance, the set {1,2,3} could be represented as the list (2 3 2 1
;; 3 2 2). Design procedures element-of-set?, adjoin-set, union-set,
;; and intersection-set that operate on this representation. How does
;; the efficiency of each compare with the corresponding procedure for
;; the non-duplicate representation? Are there applications for which
;; you would use this representation in preference to the
;; non-duplicate one?

(define (element-of-set? x set)
  (cond ((null set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2) (append set1 set2))

;; Adjoin set and union set in this representation are both
;; significantly faster. Adjoin set is constant time, while union-set
;; is linear? time.

;; element-of-set? however gets much slower, especially as sets are
;; composed.

;; A situation where this could be a better representation would be
;; when you're expecting to be building sets from a significant number
;; of sub-components, so many calls to union-set/adjoin-set, but with
;; the expectation that there will be a high amount of uniqueness
;; across these pieces.
