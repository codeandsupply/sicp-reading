;; Exercise 1.43 asks the reader to implement the nth repeated
;; application of a function f. So for n = 3, the resulting function
;; should be f(f(f(x))). For n = 1, it should give f(x), and for n = 0
;; (zero applications of f to the argument x) the result should just
;; be x. The solution to 1.43 can be written like this:

(define (repeated f n)
  (if (= n 0)
    (lambda (x) x)
    (compose f (repeated f (- n 1)))))

;; What about when n is less than 0? n = -1 applications of the
;; function f could be considered equivalent to one application of 
;; the inverse of f. It is easy to generalize the repeated function 
;; to handle negative values of n when you pass it both f and 
;; f_inverse. Here is one way:

(define (repeated f f_inverse n)
  (if (= n 0)
    (lambda (x) x)
    (if (< n 0)
      (repeated f_inverse f (- n))
      (compose f (repeated f f_inverse (- n 1)))
    )
  )
)

;; For instance, 

((repeated square sqrt -2) 625)

;; should yield 5 (to some precision).

;; An interesting challenge is to modify the above function to handle
;; negative repetitions of a function f when you don't supply an
;; f_inverse function as an argument.

;; That is,

((repeated square -2) 625)

;; should still yield 5 (again to some precision).

;; Hint: This problem is totally in the spirit of the exercises in
;; SICP Section 1.3 and can be solved by using the ideas in that
;; section.

;; =============================================================
;; A solution:

;; This problem boils down to calculating the inverse of a function
;; f, based only on the passed argument (x) and function (f).
;; In Section 1.3, the square root function is calculated
;; by first noting that the square root of a number x is the 
;; solution to the equation y^2 - x = 0.

;; You can use Newton's method to find a zero of the function
;; y ==> y^2 -x, which gives you the following definition of sqrt: 

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

;; We want to solve the equation f(y) - x = 0,
;; (e.g., find the value of y that solves the equation f(y) = 3),
;; and we can do this in the same way by defining the inverse of
;; f like this:

(define (inverse f)
  (lambda (y) 
    (newtons-method (lambda (x) (- (f x) y))
                    1.0)))

;; Then the generalized answer to Exercise 1.43 looks like this:

(define (repeated f n)
  (if (= n 0)
    (lambda (x) x)
    (if (< n 0)
      (repeated (inverse f) (- n))
      (compose f (repeated f (- n 1)))
    )
  )
)
