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

;; Baker, Cooper, Fletcher, Miller, and Smith live on different floors
;; of an apartment house that contains only five floors. Baker does
;; not live on the top floor. Cooper does not live on the bottom
;; floor. Fletcher does not live on either the top or the bottom
;; floor. Miller lives on a higher floor than does Cooper. Smith does
;; not live on a floor adjacent to Fletcher's. Fletcher does not live
;; on a floor adjacent to Cooper's. Where does everyone live?

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

;; This should produce:

'((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))


;; We can also do simple natural language parsing.

;; Start by defining examples of parts of speech:

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))

;; And then we define a grammar:

(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-word verbs)))

(define (parse-noun-phrase)
  (list 'noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))
