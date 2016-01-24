;; Exercise 3.7. Consider the bank account objects created by
;; make-account, with the password modification described in exercise
;; 3.3. Suppose that our banking system requires the ability to make
;; joint accounts. Define a procedure make-joint that accomplishes
;; this. Make-joint should take three arguments. The first is a
;; password-protected account. The second argument must match the
;; password with which the account was defined in order for the
;; make-joint operation to proceed.  The third argument is a new
;; password. Make-joint is to create an additional access to the
;; original account using the new password. For example, if peter-acc
;; is a bank account with password open-sesame, then
;;
;;     (define paul-acc
;;      (make-joint peter-acc ’open-sesame ’rosebud))
;;
;; will allow one to make transactions on peter-acc using the name
;; paul-acc and the password rosebud. You may wish to modify your
;; solution to exercise 3.3 to accommodate this new feature.


(define (make-account balance acc-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password amount)
    "Incorrect password")
  (define (dispatch password method)
    (if (eq? password acc-password)
        (cond ((eq? method 'withdraw) withdraw)
              ((eq? method 'deposit) deposit)
              (else (error "Unknown Request -- MAKE-ACCOUNT" method)))
        incorrect-password))
  dispatch)

(define (make-joint acc orig-pass add-pass)
  (define (new-acc password method)
    (if (or (eq? password orig-pass) (eq? password add-pass))
        (acc orig-pass method)
        (acc password method)))
  new-acc)

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'rosebud 'withdraw) 10)
((paul-acc 'open-sesame 'deposit) 100)
((paul-acc 'wrong 'deposit) 10)
