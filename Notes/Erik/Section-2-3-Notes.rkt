#lang planet neil/sicp

;; Section 2.3: Symbolic Data

;; With quote (') we can make lists of symbols rather than just the
;; value stored in those symbols
(define a 1)
(define b 2)
(define c 2)
(list a b)
(list 'a 'b)

(car '(a b c))
;; a
(cdr '(a b c))
;; {b c}

(eq? 'a 'a)  ;; #t
(eq? 'a 'b)  ;; #f
(eq? 'b 'c)  ;; #f


(define (memq item xs)
  (cond ((null? xs) false)
        ((eq? item (car xs)) xs)
        (else (memq item (cdr xs)))))

(memq 'a '(a b c d))
;; {a b c d}

(memq 'c '(a b c d))
;; {c d}

(memq 'x '(a b c d))
(memq 2 '(1 2 a b))


;; Symbolic Differentiation
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Wishfull Thinking:

;; (variable? e)          Is e a variable?
;; (same-variable? v1 v2) Are v1 and v2 the same variable?
;; (sum? e)               Is e a sum?
;; (addend e)             Addend of the sum e.
;; (augend e)             Augend of the sum e.
;; (make-sum a1 a2)       Construct the sum of a1 and a2.
;; (product? e)           Is e a product?
;; (multiplier e)         Multiplier of the product e.
;; (multiplicand e)       Multiplicand of the product e.
;; (make-product m1 m2)   Construct the product of m1 and m2.

;; Then we can define:

;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp)
;;          (if (same-variable? exp var) 1 0))
;;         ((sum? exp)
;;          (make-sum (deriv (addend exp) var)
;;                    (deriv (augend exp) var)))
;;         ((product? exp)
;;          (make-sum
;;           (make-product (multiplier exp)
;;                         (deriv (multiplicand exp) var))
;;           (make-product (deriv (multiplier exp) var)
;;                         (multiplicand exp))))
;;         (else
;;          (error "Unknown expression type -- DERIV" exp))))

;; We just need the proper constructors and selectors

(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? exp)
  (and (pair? exp) (eq? (car exp) '+)))
(define (product? exp)
  (and (pair? exp) (eq? (car exp) '*)))

(define addend cadr)
(define augend caddr)
(define multiplier cadr)
(define multiplicand caddr)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "Unknown expression type -- DERIV" exp))))

(deriv '(+ x 1) 'x)
(deriv '(+ (+ (+ 1 0) (* 3 x)) (* x x)) 'x)

;; While this works, it doesn't simplifiy our expressions.
;;
;; An easy way to get the simplification we want is to include it in
;; our constructors

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum-simp a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product-simp m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))


(define (deriv-simp exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum-simp (deriv-simp (addend exp) var)
                        (deriv-simp (augend exp) var)))
        ((product? exp)
         (make-sum-simp
          (make-product-simp (multiplier exp)
                             (deriv-simp (multiplicand exp) var))
          (make-product-simp (deriv-simp (multiplier exp) var)
                             (multiplicand exp))))
        (else
         (error "Unknown expression type -- DERIV" exp))))

(deriv-simp '(+ x 1) 'x)
(deriv-simp '(+ (+ 0 (* 3 x)) (* x x)) 'x)
(deriv-simp '(* x y) 'x)
(deriv-simp '(+ x (* 3 x)) 'x)


;; Representing Sets

;; Wishfull Thinking:
;;
;; union-set
;; intersection-set
;; element-of-set?
;; adjoin-set

;; Sets as unordered lists:

(define (element-of-set-uol? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set-uol? x (cdr set)))))

(define (adjoin-set-uol x set)
  (if (element-of-set-uol? x set)
      set
      (cons x set)))

(define (intersection-set-uol set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set-uol? (car set1) set2)
         (cons (car set1)
               (intersection-set-uol (cdr set1) set2)))
        (else (intersection-set-uol (cdr set1) set2))))

(define (union-set-uol set1 set2)
  (if (null? set1)
      set2
      (union-set-uol
       (cdr set1)
       (adjoin-set-uol (car set1) set2))))


;; Sets as ordered lists

;; This will give us a performance improvement. We'll deal strictly
;; with numbers, but we could do this based on lexographically
;; comparing symbols, or assigning every object a unique number.

(define (element-of-set-ol? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set-ol? x (cdr set)))))

(define (intersection-set-ol set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-ol (cdr set1) (cdr set2))))
              ((< x1 x2) (intersection-set-ol (cdr set1) set2))
              ((> x1 x2) (intersection-set-ol set1 (cdr set2)))))))


;; Sets as Binary Trees

;; We can get even more efficiency gains.

(define entry car)
(define left-branch cadr)
(define right-branch caddr)
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set-tree? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set-tree? x (left-branch set)))
        ((> x (entry set))
         (element-of-set-tree? x (right-branch set)))))

(define (adjoin-set-tree x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set-tree x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set-tree x (right-branch set))))))


;; Example Huffman Trees

;; Procedures for encoding information in a huffman tree and decoding
;; a string of bits into symbols.

;; Leaves in a huffman tree are symbols and weights
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define symbol-leaf cadr)
(define weight-leaf caddr)


(define left-branch car)
(define right-branch cadr)
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (list (weight-leaf tree))
      (cadddr tree)))

(define (make-code-tree left right)
  (list
   left
   right
   (append (symbols left) (symbols right))
   (+ (weight left) (weight right))))


;; Decoding a bit string means following left and right down the tree
;; with each bit (0 -> left, 1 -> right) until you hit a leaf.

;; This embodies the idea of a "prefix code", since only leaves have values.

(define (choose-branch bit branch)
  (cond ((= 0 bit) (left-branch branch))
        ((= 1 bit) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (adjoin-set leaf set)
  ;; Add a leaf (a pair of symbol/weight) to a set of leaves
  (cond ((null? set) (list leaf))
        ((< (weight leaf) (weight (car set))) (cons leaf set))
        (else (cons (car set)
                    (adjoin-set leaf (cdr set))))))

(define (make-leaf-set pairs)
  ;; Make a leaf-set from a sequence of symbol/frequency pairs in the form
  ;; ((A 1) (B 2) (C 3))
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pairs)     ; symbol
                               (cadr pairs))   ; frequency
                    (make-leaf-set (cdr pairs))))))
