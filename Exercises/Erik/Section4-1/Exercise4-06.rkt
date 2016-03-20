;; Exercise 4.6.  Let expressions are derived expressions, because

;; (let ((<var1> <exp1>) ... (<varn> <expn>))
;;   <body>)

;; is equivalent to

;; ((lambda (<var1> ... <varn>)
;;    <body>)
;;  <exp1>

;;  <expn>)

;; Implement a syntactic transformation let->combination that reduces
;; evaluating let expressions to evaluating combinations of the type
;; shown above, and add the appropriate clause to eval to handle let
;; expressions.

(define (let->combination exp env)
  (meta-apply (make-procedure (let-parameters exp)
                              (let-body exp)
                              env)
              (let-values exp)))

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-body exp) (caddr exp))
(define (let-arguments) (cadr exp))
(define (let-parameters exp)
  (map car (let-arguments exp)))
(define (let-values exp)
  (map cadr (let-arguments exp)))

;; Maybe instead of the above, lift
