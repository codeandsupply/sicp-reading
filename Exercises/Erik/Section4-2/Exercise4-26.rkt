;; Exercise 4.26.  Ben Bitdiddle and Alyssa P. Hacker disagree over
;; the importance of lazy evaluation for implementing things such as
;; unless. Ben points out that it's possible to implement unless in
;; applicative order as a special form. Alyssa counters that, if one
;; did that, unless would be merely syntax, not a procedure that could
;; be used in conjunction with higher-order procedures. Fill in the
;; details on both sides of the argument. Show how to implement unless
;; as a derived expression (like cond or let), and give an example of
;; a situation where it might be useful to have unless available as a
;; procedure, rather than as a special form.

;; Answer
;;;;;;;;;

;; Having unless in an applicative order evaluator works if you
;; include a special form rule for it in `eval`. You could simply do a
;; syntax transformation to an `if` clause.

(define (unless? exp) (tagged-list? exp 'unless))
(define (unless-clauses exp) (cdr exp))
(define (unless-pred exp) (cadr exp))
(define (unless-consequent exp) (caddr exp))
(define (unless-alternative exp) (cadddr exp))

(define (unless->if exp)
  ;; We can simply reverse the order of the consequent and alternative
  (make-if (unless-pred exp)
           (unless-alternative exp)
           (unless-consequent exp)))

;; However, this can't be passed around. I don't know when you'd want
;; it as a higher order procedure though.
