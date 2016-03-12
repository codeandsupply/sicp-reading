;; 4.1 Metacircular Evaluator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An evaluator written in the same language that it's evaluating is
;; called "metacircular.

;; This evaluator is constructed like our environment model of
;; evaluation. The evaluator has two core procedures: eval and
;; apply. They respectively have the tasks of:

;; To eval: eval the sub expressions, then apply the procedure to
;; them.

;; To apply: Construct an environment frame, and eval the body of the
;; procedure.


;; 4.1.1 The core of the evaluator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Eval
;;
;; - takes as arguments an environment and an expression.
;; - structured as a case analysis of the types of the expression
;;   - each type of expression has predicates that test for it

;; Eval Primitive Expressions
;;
;; - For self evaling expressions, return them
;; - Eval must look up variables in the environment to return them

;; Eval Special Forms
;;
;; - For quoted expressions, return the expression that was quoted
;; - Assignement: recursively eval rhs, modify environment to set it
;; - `if` requires special ordering of eval
;; - `lambda` must be transformed into a procedure
;; - `begin` must eval expressions in the order they appear
;; - `case` must be transformed into a series of `if`s

;; Eval Combinations
;;
;; - for procedure application, eval args, then pass to apply

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
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


;; Apply
;;
;; Also a conditional expression, that comes in with two sub
;; procedures:
;;
;; - apply-primitive-procedure
;; - apply-compount-procedure

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))
