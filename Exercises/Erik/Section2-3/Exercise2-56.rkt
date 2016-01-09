#lang planet neil/sicp

;; Exercise 2.56. Show how to extend the basic differentiator to
;; handle more kinds of expressions. For instance, implement the
;; differentiation rule
;;
;; d(u^n) / dx = n * u^(n-1) (du/dx)
;;
;; by adding a new clause to the deriv program
;; and defining appropriate procedures:
;;
;;    exponentiation?
;;    base
;;    exponent
;;    make-exponentiation
;;
;; (You may use the symbol ** to denote exponentiation.) Build in the
;; rules that anything raised to the power 0 is 1 and anything raised
;; to the power 1 is the thing itself.

;; Set Up:
;;;;;;;;;;

(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? exp)
  (and (pair? exp) (eq? (car exp) '+)))
(define (product? exp)
  (and (pair? exp) (eq? (car exp) '*)))

(define addend cadr)
(define augend caddr)
(define multiplier cadr)
(define multiplicand caddr)

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; Answer
;;;;;;;;;

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))
(define base cadr)
(define exponent caddr)
(define (make-exponentiation b exp)
  (cond ((=number? exp 1) b)
        ((=number? exp 0) 0)
        (else (list '** b exp))))


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
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        (else
         (error "Unknown expression type -- DERIV" exp))))

(deriv (make-exponentiation (make-sum (make-product 3 'x) 3) 2) 'x)
(deriv (make-sum (make-product 3 'x) 3) 'x)
