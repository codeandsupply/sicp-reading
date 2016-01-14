;; Exercise 2.57. Extend the differentiation program to handle sums
;; and products of arbitrary numbers of (two or more) terms. Then the
;; last example above could be expressed as
;;
;;     (deriv ’(* x y (+ x 3)) ’x)
;;
;; Try to do this by changing only the representation for sums and
;; products, without changing the deriv procedure at all. For example,
;; the addend of a sum would be the first term, and the augend would
;; be the sum of the rest of the terms.


(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; BEGIN ANSWER:
;;;;;;;;;;;;;;;;

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


;; END ANSWER:
;;;;;;;;;;;;;;

(define (=number? exp num)
  (and (number? exp) (= exp num)))


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
