;; Exercise 3.13.  Consider the following make-cycle procedure, which
;; uses the last-pair procedure defined in exercise 3.12:

(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)

;; Draw a box-and-pointer diagram that shows the structure z created by

(define z (make-cycle (mlist 'a 'b 'c)))

;; What happens if we try to compute (last-pair z)?

(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))

;; Answer
;;;;;;;;;

;; The program will run forever
