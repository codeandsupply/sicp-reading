#lang planet neil/sicp

;; Exercise 2.53. What would the interpreter print in response to
;; evaluating each of the following expressions?

(define (memq item xs)
  (cond ((null? xs) false)
        ((eq? item (car xs)) xs)
        (else (memq item (cdr xs)))))

(list 'a 'b 'c)
;; {a b c}

(list (list 'george))
;; {{george}}

(cdr '((x1 x2) (y1 y2)))
;; {{y1 y2}}

(cadr '((x1 x2) (y1 y2)))
;; {y1 y2}

(pair? (car '(a short list)))
;; #f

(memq 'red '((red shoes) (blue socks)))
;; #f

(memq 'red '(red shoes blue socks))
;; {red shoes blue socks}