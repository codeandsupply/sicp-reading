;; Exercise 2.64. The following procedure list->tree converts an
;; ordered list to a balanced binary tree. The helper procedure
;; partial-tree takes as arguments an integer n and list of at least n
;; elements and constructs a balanced tree containing the first n
;; elements of the list. The result returned by partial-tree is a pair
;; (formed with cons) whose car is the constructed tree and whose cdr
;; is the list of elements not included in the tree.

(define (make-tree entry left right)
  (list entry left right))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

;; a. Write a short paragraph explaining as clearly as you can how
;; partial-tree works. Draw the tree produced by list->tree for the
;; list (1 3 5 7 9 11).
;;
;; b. What is the order of growth in the number of steps required by
;; list->tree to convert a list of n elements?

;; Answer (A)
;;;;;;;;;;;;;

;; partial-tree returns a balanced tree made from the first n elements
;; of elts. It does this by dividing the elements into 3 parts, a left
;; part, a node, and a right part. It then recurses on the left and
;; right parts, assembling them into a tree.

;;      5
;;  1       9
;;    3   7   11

;; Answer (B)
;;;;;;;;;;;;;

;; I think this is linear.



;; MISC NOTES
;;;;;;;;;;;;;

;; left-size      = (quotient (- n 1) 2) 
;; left-result    = (partial-tree elts left-size)
;; left-tree      = (car left-result)
;; non-left-elts  = (cdr left-result)
;; right-size     = (- n (+ left-size 1))
;; this-entry     = (car non-left-elts))
;; right-result   = (partial-tree (cdr non-left-elts) right-size)
;; right-tree     = (car right-result)
;; remaining-elts = (cdr right-result)

;; (cons (make-tree this-entry left-tree right-tree) remaining-elts)

;; left-size = 2
;; left-result = (partial-tree elts 2)

;; left-tree = (car left-result)

;; non-left-elts = (cdr left-result)
;; right-size = 3
;; this-entry = (car non-left-elts)
;; right-result = (partial-tree (cdr non-left-elts) right-size)

;; right-tree = (car right-result)

;; remaining-elts = (cdr right-result)

;; (cons (make-tree this-entry left-tree right-tree) reamining-elts)

;; partial-tree returns a balanced tree made from the first n elements of
;; elts. It does this by dividing the elements into 3 parts, a left part,
;; a node, and a right part. It then recurses on the left and right
;; parts, assembling them into a tree.

;; partial-tree, in every iteration, will return either ('() . elts) or
;; (some-tree . elts). That is, it returns a dotted pair of a tree (which
;; can be null), and the remaining elements.
