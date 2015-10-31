#lang planet neil/sicp

;; Section 2.1 Introduction to Data Abastraction

;; 2.1.1 Example Arithetic Operations for Rational Numbers

;; Assume we have:
;;
;;     (make-rat n d)
;;     (numer x)
;;     (denom x)

;; Given the above, we can define, add-rat, sub-rat, mul-rat, div-rat, equal-rat?

;; First, I'm going to cheat, so I can load this file. Ignore:
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
;; end ignore

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y)
        (denom x) (numer y))))

;; Pairs
(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))
(car (car z))
(car (cdr z))

;; With cons, car and cdr we can define make-rat, numer, and denom
;; (define (make-rat n d) (cons n d))
;; (define (numer x) (car x))
;; (define (denom x) (cdr x))

;; And a way to display
(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

;; Trying it out
(define one-half (make-rat 1 2))
(print-rat one-half)
(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

;; We can fix make-rat to simplify
(define (make-rat-simplify n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))


;; Abstraction Barriers

;; Create levels of abstraction, with interfaces that only are used
;; one level up. Allows you to defer implementation tradeoffs until
;; later.

;; What is Meant by Data?

;; Data is defined by some collection of selectors and constructors,
;; together with specified conditions that these procedures must
;; fulfill in order to be a valid representation.

;; We can redefine cons, car, and cdr with this in mind
(define (my-cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)
(define (my-car z) (z 0))
(define (my-cdr z) (z 1))


;; 2.1.4 Extended Exercise: Interval Arithmetic

;; We imagine an object that is an interval, with a start and end
;; point, and want to do arithmetic on them

(define (make-interval a b) (cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (upper-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
