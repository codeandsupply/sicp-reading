;; Exercise 3.6. It is useful to be able to reset a random-number
;; generator to produce a sequence starting from a given value. Design
;; a new rand procedure that is called with an argument that is either
;; the symbol generate or the symbol reset and behaves as follows:
;;
;;     (rand ’generate)
;;
;; produces a new random number;
;;
;;     ((rand ’reset) <new-value>)
;;
;; resets the internal state variable to the designated
;; <new-value>. Thus, by resetting the state, one can generate
;; repeatable sequences. These are very handy to have when testing and
;; debugging programs that use random numbers.


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
    (define (generate)
      (set! x (rand-update x))
      x)
    (define (reset new-x)
      (set! x new-x))
    (define (dispatch method)
      (cond ((eq? method 'generate) (generate))
            ((eq? method 'reset) reset)
            (else (error "Unexpected method -- RAND" method))))
    dispatch))


((rand 'reset) 1000)
(rand 'generate)
(rand 'generate)
(rand 'generate)
((rand 'reset) 1000)
(rand 'generate)
(rand 'generate)
(rand 'generate)
