;; Numbers:

(+ 137 349)
(- 1000 334)
(* 5 99)
(/ 10 3)
(+ 2.7 3)
(+ 1 2 3 3)
(* 25 2 2)
(- 1 1 1)

(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))


;; Names:

(define size 5)
(* size 2)


;; Evaluation Rules
;;
;; - Apply
;; - Recursively
;; - All expressions have a value
;; - Follows 3 rules
;;   1. Numerals have value of the number they name
;;   2. Operators have value equal to their machine code
;;   3. Other names have values as defined by their environment
;; - Some expressions don't follow the three rules
;;   - Like `define`
;;   - Called "special forms"


;; Defining Procedures:

(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))


;; Conditionals

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

; Incomplete conditions return "Unspecified return value"
(define (absbad x)
  (cond ((> x 0) x)
        ((= x 0) 0)))

(define (abs2 x)
  (cond ((< x 0) -x)
        (else x)))

(define (abs3 x)
  (if (< x 0)
      (- x)
      x))

(define x 3)
(and (> x 5) (= x 7))
(or (> x 10) (< x -10))
(not (= x 4))


;; Square Root Approximation

;; To improve a guess of a square root for some number, x, we can take
;;
;;   (guess + x / guess) / 2


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x) (sqrt-iter 1.0 x))


;; Black Boxes and Formal Parameters

;; Bound vs Free
;; - formal parameters are "bound" to their name
;; - other parameters are free
;; 
;; consider the following function

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;; In the body of this function `guess` and `x` are bound while `<`,
;; `abs`, and `square` are all free.

;; The set of expressions for which a binding defines a name is called
;; the "scope" of that name.


;; Block Structure

;; We can simplify out definition of sqrt, and expunge the extraneous
;; names by including the definitions within the function

(define (sqrt x)
  (define (sqrt-iter guess)
    (if (good-enough? guess) 
        guess
        (sqrt-iter (improve guess))))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.0001))
  (define (square n) (* n n))
  (define (improve guess)
    (average guess (/ x guess)))
  (sqrt-iter 1.0))

;; Note that the inner function definitions to not explicitly require
;; any formal argument `x`, since `x` will be available through
;; lexical scoping.
