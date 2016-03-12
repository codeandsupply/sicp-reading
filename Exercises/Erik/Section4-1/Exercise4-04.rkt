;; Exercise 4.4.  Recall the definitions of the special forms and and
;; or from chapter 1:

;; and: The expressions are evaluated from left to right. If any
;; expression evaluates to false, false is returned; any remaining
;; expressions are not evaluated. If all the expressions evaluate to
;; true values, the value of the last expression is returned. If there
;; are no expressions then true is returned.

;; or: The expressions are evaluated from left to right. If any
;; expression evaluates to a true value, that value is returned; any
;; remaining expressions are not evaluated. If all expressions
;; evaluate to false, or if there are no expressions, then false is
;; returned.

;; Install and and or as new special forms for the evaluator by
;; defining appropriate syntax procedures and evaluation procedures
;; eval-and and eval-or. Alternatively, show how to implement and and
;; or as derived expressions.

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;; Tests / Selectors
(define (and? exp) (tagged-list? exp 'and))
(define (and-conditions exp) (cdr exp))

(define (or? exp) (tagged-list? exp 'or))
(define (or-conditions exp) (cdr exp))

(define (first-cond conditions) (car conditions))
(define (rest-conds conditions) (cdr conditions))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; Evaluators
(define (eval-and exp env)
  (eval-and-conds (and-conditions exp) env))
(define (eval-and-conds conds env)
  (cond ((null? (first-cond conds)) true)
        ((true? (eval (first-cond conds) env))
         (eval-and-conds (rest-conds conds)))
        (else false)))

(define (eval-or exp env)
  (eval-or-conds (or-conditions exp) env))
(define (eval-or-conds conds env)
  (cond ((null? (first-cond conds)) false)
        ((true? (eval (first-cond conds) env)) true)
        (else (eval-or-conds (rest-conds conds)))))
