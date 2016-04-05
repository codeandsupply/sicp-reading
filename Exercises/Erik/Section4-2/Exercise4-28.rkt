;; Exercise 4.28.  Eval uses actual-value rather than eval to evaluate
;; the operator before passing it to apply, in order to force the
;; value of the operator. Give an example that demonstrates the need
;; for this forcing.

;; Answer
;;;;;;;;;

((car (list + -)) 1 1)

;; If you simply passed the first argument to `apply` you would get an
;; error since '(car (list + -)) is neither a primitive procedure or
;; compound procedure. Forcing the actual value of this, returns `+`
;; which is a primitive procedure.
