#lang planet neil/sicp

;; Section 2.2 Hierarchical Data and the Closure Property

;; In general, an operation for combining data objects satisfies the
;; closure property if the results of combining things with that
;; operation can themselves be combined using the same operation
;;
;; Pairs are an example of this, since you can make pairs of pairs


;; Section 2.2.1 Representing Sequences

(cons 1 (cons 2 (cons 3 (cons 4 nil))))
(list 1 2 3 4)
(define one-through-four (list 1 2 3 4))
(car one-through-four)
(car (cdr one-through-four))
(cons 10 one-through-four)

;; We can define some operations on these forms

(define (list-ref items n)
  ;; Return the nth value of the list
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))


;; If we're cdring down a whole list, we can check if we've hit the
;; end with null?
(null? nil)

(define (length items)
  ;; Return the number of items in the list
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (length2 items)
  (define (length-iter items acc)
    (if (null? items)
        acc
        (length-iter (cdr items) (+ 1 acc))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (append (cdr list1) (cons (car list1) list2))))


;; Mapping over lists

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

;; We can abstract this pattern into a higer order procedure

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(map abs (list 1 -2 3 -4))
(map (lambda (x) (* x x)) (list 1 2 3 4))

;; We can now redefine scale-list in terms of map

(define (scale-list2 items factor)
  (map (lambda (x) (* x factor)) items))


;; Section 2.2.2 Hierarchical Structures

;; We can think of lists of lists as tree structures. Here, recursion
;; is a natural way to work with these data structures, since we can
;; operate on the tree, and then recurse on each sub-tree.

;; To aid in writing recursive processes on trees, scheme has pair?

(define x (cons (list 1 2) (list 2 4)))
(length x)  ;; 3

(pair? (cons 1 1))
(pair? nil)
(pair? 1)

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(count-leaves x)


;; Mapping over trees

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(scale-tree x 10)

(define (scale-tree2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree2 sub-tree factor)
             (* sub-tree factor)))
       tree))

(scale-tree2 x 10)


;; 2.2.3 Sequences as Conventional Interfaces
