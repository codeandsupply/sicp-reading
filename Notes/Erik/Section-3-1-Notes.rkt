;; Chapter 3 Modularity, Objects, and State
;;
;; We will be looking at how to design large complex pieces of
;; software, and how to introduce modularity. We will specifically
;; look at two approaches to modularity:
;;
;;    1) Objects
;;    2) Streams

;; Section 3.1 Assignment and Local State

;; We characterize and objects state by one or more _state variables_,
;; which taken together contain enough information to about the
;; object's history to determine it's current behavior.

;; We will model a bank acount with a function:
;;
;;    (withdraw <amount>)
;;
;; Which returns the remaining balance or "Insuficient funds". Note
;; that multiple calls to withdraw with the same argument can produce
;; different results.

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient Funds"))

;; Here we use the `begin` special form, to evaluate multiple
;; expressions, and return the result of the last one.
;;
;; Note, we've been using this implicitly in function definition
;; bodies, and the <consequent> portion of `cond` clauses.

;; The above relies on global state, we can move this state internal
;; to a function:

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds!")))


;; We can extend this further to make a full account object

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insuficient funds!"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

;; Which can be used as such:
(define acc (make-account 100))
((acc 'withdraw) 100)
((acc 'deposit) 1000)
((acc 'withdraw) 2000)
;; ((acc 'boop) 10)


;; 3.1.2 The Benefits of introducing assigment

;; We can write a function that needs to store state, like a prng, as
;; follows:

(define random-init 1)
(define (rand-update x)
  ;; This is a linear congruential generator
  ;; I wrote it because it's easy to implement
  ;; See: https://en.wikipedia.org/wiki/Linear_congruential_generator
  (let ((a 1103515245)
        (m (expt 2 32))
        (c 12345))
    (modulo (+ (* x a) c) m)))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

;; This function uses a rand-update function that moves through a
;; chain of numbers in a way that behaves statistically like a random
;; number.
;;
;; Without keeping internal state, we would be required to hold
;; references to the current place in the prng chain any time we
;; wanted to get random numbers.

;; We can show the difference in implementation simplicity of a
;; Monte-Carlo simulation with the internal state of rand vs no
;; internal state, just using rand-update.

;; We're going to estimate pi! 6/(pi^2) = the probability that two
;; integers choosen at random are co-prime.

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  ;; Return the fraction of trials that pass the experiment
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;; Unfortunately this estimate isn't great, because of our prng
;; This could make it better
(define (cesaro-test-sysrand)
  (let ((rand (lambda () (random 4294967087))))
    (= (gcd (rand) (rand)) 1)))


;; To do this without keeping state in rand:
(define (estimate-pi-2 trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))
(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)
                     x2))
              (else
               (iter (- trials-remaining 1)
                     trials-passed
                     x2))))))
  (iter trials 0 initial-x))


;; Keeping track of random number state across iterations requires
;; moving the experiment into the monte-carlo code, rather than
;; keeping it nicely modularized.

;; 3.1.3 The Costs of Introducing Assignment

;; By introducing assignment, we can no longer use the substitution
;; model for analyzing the values returned by functions.
;;
;; This is due to the fact that the substitution model relied on names
;; being shorthand for values. However, when those values can change
;; over time (over the course of the program) we need a new model to
;; handle that change.

;; A language that supports the concept "equals can be substituted for
;; equals" in an expression, without changing the value, is said to be
;; "referentially transparent".

;; Once we forego referential transparency, the idea of equality
;; becomes much harder to pin down.
