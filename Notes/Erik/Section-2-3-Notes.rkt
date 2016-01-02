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
