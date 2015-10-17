#lang planet neil/sicp

;; Exercise 1.20. The process that a procedure generates is of course
;; dependent on the rules used by the interpreter. As an example,
;; consider the iterative gcd procedure given above. Suppose we were
;; to interpret this procedure using normal-order evaluation, as
;; discussed in section 1.1.5. (The normal-order-evaluation rule for
;; if is described in exercise 1.5.) Using the substitution method
;; (for normal order), illustrate the process generated in evaluating
;; (gcd 206 40) and indicate the remainder operations that are
;; actually performed. How many remainder operations are actually
;; performed in the normal-order evaluation of (gcd 206 40)? In the
;; applicative-order evaluation?


;; Answer
;;;;;;;;;

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Normal order
(gcd 206 40)

(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))

(if (= 40 0)
    206
    (if (= (remainder 206 40) 0)
        40
        (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))))

(if (= 40 0)
    206
    (if (= (remainder 206 40) 0)
        40
        (if (= (remainder 40 (remainder 206 40)))
            (remainder 206 40)
            (gcd (remainder 40 (remainder 206 40))
                 (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))

;; etc. etc. There are lots


;; Four remainder calls

;; Applicative Order
(gcd 206 40)
  (gcd 40 (remainder 206 40))
(gcd 40 6)
  (gcd 6 (remainder 40 6))
(gcd 6 4)
  (gcd 4 (remainder 6 4))
(gcd 4 2)
  (gcd 2 (remainder 4 2))
(gcd 2 0)

;; Four remainder calls
