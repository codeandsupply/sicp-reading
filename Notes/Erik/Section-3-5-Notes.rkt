;; 3.5 Streams
;;;;;;;;;;;;;;

;; Streams are objects with "delayed evaluation"

;; Lets us build large, potentially infinite list analogs
;; We can interleave steps of computation

;; 3.5.1 Streams are Delayed Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; primitives:

;; (stream-car (cons-stream x y)) ;; x
;; (stream-cdr (cons-stream x y)) ;; y
;; (stream-null? the-empty-stream) ;; #t

(require racket/stream)
;;   - this gives us a lot of the same primitives
;;   - in their actual special form form

;; We can build some functions we're used to:

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
;; (define (stream-map proc s)
;;   (if (stream-null? s)
;;       empty-stream
;;       (stream-cons (proc (stream-car s))
;;                    (stream-map proc (stream-cdr s)))))
;; The above is generalized below, so it's commented out here
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (display x)
  (newline))


;; To implement streams with an efficiency bonus, and to interleave
;; computation, we're going to set things up so the cdr is only
;; evaluatated at access time.

;; With ordinary lists, both the car and the cdr are evaluated at
;; construction time. With streams, the cdr is evaluated at selection
;; time.

;; To primitivs:
;;
;;   (delay <expression>)
;;   - returns a promise to evaluate that procedure later
;;
;;   (force promise)
;;   - evaluates the procedure in the promise right now


;; (define (cons-stream a b) (cons a (delay b)))
;; (define the-empty-stream 'the-emtpy-stream)
;; (define (stream-null? stream) (eq? stream 'the-emtpy-stream))
;; (define (stream-car stream) (car stream))
;; (define (stream-cdr stream) (force (cdr stream)))
;;    - These are how we want things to behave, but we need special forms


(define (stream-car stream) (stream-first stream))
(define (stream-cdr stream) (stream-rest stream))
(define stream-null? stream-empty?)

;; We can now define some procedures that use this stuff

(define (stream-enumerate-interval low high)
  (if (> low high)
      empty-stream
      (stream-cons low (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) empty-stream)
        ((pred (stream-car stream))
         (stream-cons (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


(define (take n stream)
  (if (equal? n 0)
      '()
      (cons (stream-car stream)
            (take (- n 1) (stream-cdr stream)))))

(define (odd? n) (not (equal? 0 (remainder n 2))))

;; In general, we can think of delayed evaluation as ``demand-driven'' programming

;; We can think of implementing delay and force follows: Delay is
;; simply syntatic sugar for taking an expresion and returning a
;; lambda that evaluates to calling that expression (and memoizing the result).
;; Force just calls the lambda.

;; 3.5.2 Infinite Streams
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens (stream-filter (lambda (x) (not (divisible? x 7))) integers))

;; Generating a stream of primes

(define (sieve stream)
  (stream-cons
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

;; The above is interesting because the generating sequence defines an
;; infinite list of primes, as a stream which contains an infinite
;; list of sieves.

;; We can also define streams implicitly

(define ones (stream-cons 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))
;;   - requires the generalized stream map:
(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))


(define fibs
  (stream-cons 0
               (stream-cons 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))


;; 3.5.3 Exploiting the Stream Paridigm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Formulating Iterations as stream procedures

(define (average a b) (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (stream-cons 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)
(take 5 (sqrt-stream 2))


;; Infinite streams of pairs

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (stream-cons
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))
