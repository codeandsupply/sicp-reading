;; Exercise 4.52.  Implement a new construct called if-fail that
;; permits the user to catch the failure of an expression. If-fail
;; takes two expressions. It evaluates the first expression as usual
;; and returns as usual if the evaluation succeeds. If the evaluation
;; fails, however, the value of the second expression is returned, as
;; in the following example:

;;; Amb-Eval input:
(if-fail (let ((x (an-element-of '(1 3 5))))
           (require (even? x))
           x)
         'all-odd)
;;; Starting a new problem
;;; Amb-Eval value:
all-odd
;;; Amb-Eval input:
(if-fail (let ((x (an-element-of '(1 3 5 8))))
           (require (even? x))
           x)
         'all-odd)
;;; Starting a new problem
;;; Amb-Eval value:
8


;; Answer
;;;;;;;;;

(define (analyze-if-fail exp)
  (let ((cproc (analyze (if-fail-consequent exp)))
        (aproc (analyze (if-fail-alternative exp))))
    (lambda (env succeed fail)
      (cproc env
             (lambda (val fail2)
               val)
             (lambda () (aproc env success fail))))))
