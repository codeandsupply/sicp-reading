#lang planet neil/sicp

;; Section 2.3: Symbolic Data

;; With quote (') we can make lists of symbols rather than just the
;; value stored in those symbols
(define a 1)
(define b 2)
(define c 2)
(list a b)
(list 'a 'b)

(car '(a b c))
;; a
(cdr '(a b c))
;; {b c}

(eq? 'a 'a)  ;; #t
(eq? 'a 'b)  ;; #f
(eq? 'b 'c)  ;; #f


(define (memq item xs)
  (cond ((null? xs) false)
        ((eq? item (car xs)) xs)
        (else (memq item (cdr xs)))))

(memq 'a '(a b c d))
;; {a b c d}

(memq 'c '(a b c d))
;; {c d}

(memq 'x '(a b c d))
(memq 2 '(1 2 a b))


