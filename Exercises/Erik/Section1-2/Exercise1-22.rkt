#lang planet neil/sicp

;; Exercise 1.22. Most Lisp implementations include a primitive called
;; runtime that returns an integer that specifies the amount of time
;; the system has been running (measured, for example, in
;; microseconds). The following timed-prime-test procedure, when
;; called with an integer n, prints n and checks to see if n is
;; prime. If n is prime, the procedure prints three asterisks followed
;; by the amount of time used in performing the test.

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime))
  (newline))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (square x) (* x x))

(define (prime? n)
  (= (smallest-divisor n) n))

;; Using this procedure, write a procedure search-for-primes that
;; checks the primality of consecutive odd integers in a specified
;; range. Use your procedure to find the three smallest primes larger
;; than 1000; larger than 10,000; larger than 100,000; larger than
;; 1,000,000. Note the time needed to test each prime. Since the
;; testing algorithm has order of growth of 0(sqrt(n)), you should
;; expect that testing for primes around 10,000 should take about
;; sqrt(10) times as long as testing for primes around 1000. Do your
;; timing data bear this out? How well do the data for 100,000 and
;; 1,000,000 support the sqrt(n) prediction? Is your result compatible
;; with the notion that programs on your machine run in time
;; proportional to the number of steps required for the computation?


(define (search-for-primes start)
  (if (even? start) (search-for-primes (+ 1 start))
      (if  (prime? start)
           start
           (search-for-primes (+ 2 start)))))

; Above 1,000:
;     1009, 1013, 1019
; Above 10,000:
;     10007, 10009, 100037
; Above 100,000:
;     100003, 100019, 100043
; Above 1,000,000:
;     1000003, 1000033, 1000037

;; Timing Tests
;
; 1009 *** 8
; 1013 *** 8
; 1019 *** 8
; 10007 *** 13
; 10009 *** 12
; 10037 *** 13
; 100003 *** 28
; 100019 *** 28
; 100043 *** 29
; 1000003 *** 79
; 1000033 *** 80
; 1000037 *** 80

;; Order of growth
;
; sqrt(10) = 3.16
; 8 * 3 = 24
; 13 * 3 = 39
; 28 * 3 = 84

; These are roughly similar to what we would expect. There might be
; constant factors not being accounted for precisely. The
; proportionality is definitely there.
