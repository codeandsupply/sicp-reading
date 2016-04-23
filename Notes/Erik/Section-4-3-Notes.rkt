;; 4.3 Variations on a Scheme -- Nondeterministic Computing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Non-deterministic computing can be thought of as exploring all
;; possible worlds that we've specified trying to meet a certain set
;; of requirements. E.g.

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))

;; 4.3.1 Amb and Search
;;;;;;;;;;;;;;;;;;;;;;;

;; Two primitives: `amb` and `require`.

;; `amb`: a special form that chooses an item from its arguments
;; "ambiguously," and fails if called with no argument.

(define (require p)
  (if (not p) (amb)))

;; Then we can implement an-element-of

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

;; Or an infinite generator

(define (an-integer-starting-with n)
  (amb n (an-integer-starting-with (+ n 1))))

;; In both of these cases amb returns a single item.  We can say that
;; `amb` represents a "nondeterministic choice point".

;; When using `amb` in a driver loop, we only get one example of a
;; passing test case. To try to get another, by backtracking, we can
;; run `(try-again)`.

;; 4.3.2 Examples of Non-deterministic programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here we can solve the multiple dwelling problem:

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;; This should produce
