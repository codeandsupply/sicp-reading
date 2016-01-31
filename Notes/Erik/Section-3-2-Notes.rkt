;; 3.2 The Environment Model of Evaluation

;; 3.2.1 The Rules for evaluation

;; The environment model of procedure application can be summarized as
;; follows:

;; - A procedure object is applied to a set of arguments by:
;;   1) Constructing a frame
;;   2) Binding the formal parameters to the given values in the frame
;;   3) Evaluating the body of the procedure in that environment
;;
;; - A procedure object is created by:
;;   1) Evaluating a lambda expression in an environment
;;   2) Creating a pair of the (lambda-text . pointer-to-env)
;;   3) Storing that pair
;;
;; - The operation (set! <variable> <value>) is defined as:
;;   1) Try to locate a binding for variable in the environment chain
;;   2) If it's found change that binding to the new value
;;   3) Otherwise throw an error

;; 3.2.2 Applying Simple Procedures

;; This just had a worked example of the above.

;; 3.2.3 Frames as the repository of local state

;; 3.2.4 Internal Definitions

;; Internal definitions cause
