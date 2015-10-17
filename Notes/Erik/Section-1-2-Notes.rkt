#lang planet neil/sicp

;; Factorials

; Obvious definition
;
; A linear-recursive process
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

; Iteration version
;
; A linear-iterative process
(define (factorial2 n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

; Recursive Process vs Procedure
;
; Recursive Procedure
;  - A procedure that refers to itself syntactically
; Recursive Process
;  - The process evolution evolves recursively (say, storing data on the stack)

; Tail recursion
;
;  - An interpreter that implements tail-recursion can run iterative
;  processes in constant space


;; Tree Recursion

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

; The above definition will take a long time for large numbers. Try
; (fib 40). This is because we duplicate work as we decend into the
; tree. The run time is exponential.

; We can solve this:

(define (fib2 n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
; Think of `a` as "last" and `b` as accumulator (acc)


;; Counting Change

;; What is the number of ways to make change for a given amount of
;; money?
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount 
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))


;; Orders of Growth

;; We can described resources used for a problem size `n` as R(n). We
;; describe the order of growth as R(n) = \theta(f(n)) if there are
;; positive constants k_1 and k_2 such that:
;;
;;    k_1 * f(n) <= R(n) <= k_2 * f(n)
;;
;; for sufficently large n.


;; Exponentiation

; compute b^n

;; O(n) time, O(n) space
(define (myexpt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;; O(n) time, O(1) space
(define (myexpt2 b n)
  (myexpt2-iter b n 1))
(define (myexpt2-iter b count product)
  (if (= count 0)
      product
      (myexpt2-iter b (- count 1) (* product b))))

;; O(log(n)) time, O(log(n)) space
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))


;; Greatest Common Divisors

; GCD(a, b) = GCD(b, r)
; where r is the remainder of a/b

; Iterative process, O(log(n)) time, O(1) space
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


;; Testing for Primality

; A number is prime if it is its own smallest divisor

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

;; This has run time O(sqrt(n)). This is because we only test divisors
;; up to the the sqrt(n)
(define (prime? n)
  (= (smallest-divisor n) n))


;; Fermat’s Little Theorem: If n is a prime number and a is any
;; positive integer less than n, then a raised to the nth power is
;; congruent to a modulo n.

;; Two numbers are said to be congruent modulo n if they both have the
;; same remainder when divided by n. The remainder of a number a when
;; divided by n is also referred to as the remainder of a modulo n, or
;; simply as a modulo n.


;; Compute the exponential of a number, mod another number
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; (fast-prime? 393342743 20) ; #t

; The numbers that fool this tests are called Carmichael numbers
; (prime? 2465) ; #f
; (fast-prime? 2465 20) ; #t
; They're very rare at large numbers


;; Probababalistic Methods

; The existence of tests for which one can prove that the chance of
; error becomes arbitrarily small has sparked interest in algorithms
; of this type, which have come to be known as probabilistic
; algorithms.
