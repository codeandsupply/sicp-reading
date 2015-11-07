#lang planet neil/sicp

(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (textbook-sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define (sqratio-sqrt x)
  (define (good-enough? guess)
    (let (
          (gxratio (/ (square guess) x))
          )
      (and (> gxratio 0.99999)
           (< gxratio 1.00001))))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

; The good-enough? test used in computing square roots will not be very effective for finding the square roots of very small numbers. Also, in real computers, arithmetic operations are almost always performed with limited precision. This makes our test inadequate for very large numbers. Explain these statements, with examples showing how the test fails for small and large numbers. An alternative strategy for implementing good-enough? is to watch how guess changes from one iteration to the next and to stop when the change is a very small fraction of the guess. Design a square-root procedure that uses this kind of end test. Does this work better for small and large numbers?

(define (sqrtdeltaratio-sqrt x)
  (define (good-enough? guess previous)
    (let (
          (ratio (/ guess previous))
          )
      (and (> ratio 0.99999)
           (< ratio 1.00001))))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess previous)
    (if (good-enough? guess previous)
        guess
        (sqrt-iter (improve guess) guess)))
  (sqrt-iter 1.0 0.0))

(define (run-each-sqrt x)
  (display "Input: ") (display x) (newline)
  (map (lambda (fpair)
         (let (
               (fname (car fpair))
               (f (car (cdr fpair)))
               )
           (display fname)
           (display ":	")
           (display (f x))
           (newline)
           )
        )
       (list
         (list 'textbook-sqrt textbook-sqrt)
         (list 'sqratio-sqrt sqratio-sqrt)
         (list 'sqrtdeltaratio-sqrt  sqrtdeltaratio-sqrt)))
  (newline)
  )

(define (run-basic-demo)
  (map run-each-sqrt (list 4 9 10 1000 9999999999 1.1 1.00001 0.9999 0.000005)))
