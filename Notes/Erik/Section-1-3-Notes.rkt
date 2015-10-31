#lang planet neil/sicp

;; Section 1.3: Formulating Abstractions with Higher Order Procedures

;; Procedures as arguments

;; inclusive sum
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (cube x) (* x x x))
(define (square x) (* x x))
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

;; The procedures above look very similar, we can abstract them
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

;; Then we can redefine sum-cubes (or other functions) easily
(define (inc x) (+ x 1))
(define (sum-cubes2 a b)
  (sum cube a inc b))

(define (pi-sum2 a b)
  (define (pi-term a)
    (/ 1.0 (* a (+ a 2))))
  (define (pi-next a)
    (+ a 4))
  (sum pi-term a pi-next b))

;; The integral of a function between a and b can be numerically
;; approximated:
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))


;; Construction Procedures using Lambda
(lambda (x) (+ x 4))
(lambda (x) (/ 1.0 (* x (+ x 2))))

(define (pi-sum3 a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

((lambda (x y z) (+ x y (cube z))) 1 2 3)

;; Using Let to create local variables

; If we want to define:
;
; f(x, y) = x(1 + xy)^2 + y(1 - y) + (1 + xy)(1 - y)
;
; or
;
; a = 1 + xy
; b = 1 - y
; f(x y) = xa^2 + yb + ab
;
; We could write:
(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y)) (- 1 y)))

; which is the same as:
(define (f2 x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y)) (- 1 y)))

; which is the same as
(define (f3 x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;; In fact, the let binding is a special form that simply transforms
;; its input into the equivalent lambda expression and call


;; Procedures as General Methods

; Findind roots of equations by the half interval method
(define (search f neg-point pos-point)
  (let ((midpoint (average pos-point neg-point)))
    (if (close-enough? pos-point neg-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))
(define (average x y) (/ (+ x y) 2.0))
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-val (f a))
        (b-val (f b)))
    (cond ((and (negative? a-val) (positive? b-val))
           (search f a-val b-val))
          ((and (positive? a-val) (negative? b-val))
           (search f b-val a-val))
          (else
           (error "Values are not of opposite signs" a b)))))


;; Finding fixpoints for functions
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1) ;; 0.73908...

;; A reasonable-ish definition for sqrt that fails to converge
;;
;; This is due to the fact that we're trying to find some y, such that
;; y^2=x. This means, we're trying to find the fixed
;; point of y -> x/y, for given x.
(define (sqrt x)
  (fixed-point (lambda (y) (/ x y))
               1.0))

;; We can make this converge by using an average
(define (sqrt-b x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))


;; Procedures as Return Values

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt-c x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
