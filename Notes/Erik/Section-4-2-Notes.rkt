;; 4.2 Variations on a Scheme -- Lazy Evaluator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 4.2.1 Normal Order vs Applicative Order
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Recall that normal order enters the procedure before evaluating
;; arguments, while applicative order evaluates the arguments before
;; passing them in.

;; 4.2.2 An interpreter with Lazy Evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We'll need to store and pass around "thunks" which are arguments
;; packaged up with the environement that they need to be evaluated
;; in.
