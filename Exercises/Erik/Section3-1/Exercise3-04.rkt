;; Exercise 3.4. Modify the make-account procedure of exercise 3.3 by
;; adding another local state variable so that, if an account is
;; accessed more than seven consecutive times with an incorrect
;; password, it invokes the procedure call-the-cops.

(define (make-account balance acc-password)
  (define failed-attempts 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password amount)
    (format "Incorrect password. ~s failed attempts" failed-attempts))
  (define (dispatch password method)
    (cond ((eq? password acc-password)
           (set! failed-attempts 0)
           (cond ((eq? method 'withdraw) withdraw)
                 ((eq? method 'deposit) deposit)
                 (else (error "Unknown Request -- MAKE-ACCOUNT" method))))
          ((< failed-attempts 7)
           (set! failed-attempts (+ failed-attempts 1))
           incorrect-password)
          (else (call-the-cops))))
  dispatch)

(define acc (make-account 100 'abcdefg))
((acc 'abcdefg 'withdraw) 10)
((acc 'abcdefg 'deposit) 100)
((acc 'xyz 'withdraw) 10)

