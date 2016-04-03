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


;; 4.1.2 Representing Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The result of the operation is the result of recursively operating
;; on sub-expressions until we reach some primitive values.

;; - The only self-evaluating items are numbers and strings
;; - Variables are represented by symbols
;; - Quotations have the form (quote <text-of-quotation>)
;; - Assignments have the form (set! <var> <value>)
;; - Definitions have the form (define <var> <value>)
;;   - or (define (<var> <parameter1> ... <parametern>)
;;                <body>)
;; - Lambda expressions are lists that begin with the symbol lambda
;; - Begin packages a sequence of expressions into a single expression.
;; - cond expressions are tagged with `'cond`
;;   - We transform them into `if` statements though
;;
;; - The `tagged-list?` procedure helps us identify tagged types
;;   - like quote, set!, etc
;;
;; - The `sequence->exp` procedure helps us with let/define/cond ->
;;   begin
;;
;; - A procedure application is any compound expression that is not
;;   one of the above expression types.



;; 4.1.3 Evaluator Data Structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Operations on environments
;;
;; - (lookup-variable <var> <env>)
;; - (extend-environment <variables> <values> <base-env>)
;; - (define-variable! <var> <value> <env>)
;; - (set-variable-value! <var> <value> <env>)
;;
;; We represent an environment as a list of frames. The enclosing
;; environment of an environment is the cdr of the list. The empty
;; environment is simply the empty list.


;; 4.1.4 Running the Evaluator as a Program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 4.1.5 Data as Programs
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Think of our evaluator as a universal machine

;; Sometimes it can even be usefull to have `eval` within a program,
;; respective to our environment.

;; 4.1.6 Internal Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We should "scan out" internal definitions, by doing a
;; transformation on lambdas:

;; (lambda <vars>
;;   (define u <e1>)
;;   (define v <e2>)
;;   <e3>)

;; Becomes:

;; (lambda <vars>
;;   (let ((u '*unassigned*)
;;         (v '*unassigned*))
;;     (set! u <e1>)
;;     (set! v <e2>)
;;     <e3>))


;; 4.1.7 Separating Syntactic Analysis from Execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
