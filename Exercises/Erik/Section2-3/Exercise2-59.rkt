;; Exercise 2.59. Implement the union-set operation for the
;; unordered-list representation of sets.

(define (element-of-set-uol? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set-uol? x (cdr set)))))

(define (adjoin-set-uol x set)
  (if (element-of-set-uol? x set)
      set
      (cons x set)))

(define (union-set-uol set1 set2)
  (if (null? set1)
      set2
      (union-set-uol
       (cdr set1)
       (adjoin-set-uol (car set1) set2))))

(union-set-uol (list 2 4 8 3) (list 5 8))
