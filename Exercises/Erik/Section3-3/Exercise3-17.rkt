;; Exercise 3.17.  Devise a correct version of the count-pairs
;; procedure of exercise 3.16 that returns the number of distinct
;; pairs in any structure. (Hint: Traverse the structure, maintaining
;; an auxiliary data structure that is used to keep track of which
;; pairs have already been counted.)

(require scheme/mpair)

(define (count-pairs x)
  (define distinct-pairs '())
  (define (new? pair)
    (define (in-list? pair l)
      (cond ((null? l) #f)
            ((eq? pair (mcar l)) #t)
            (else (in-list? pair (mcdr l)))))
    (not (in-list? pair distinct-pairs)))
  (define (count-distinct-pairs x)
    (if (mpair? x)
        (cond ((new? x)
               (writeln x)
               (set! distinct-pairs (mcons x distinct-pairs))
               (+ 1
                  (count-distinct-pairs (mcar x))
                  (count-distinct-pairs (mcdr x))))
              (else 0))
        0))
  (count-distinct-pairs x))

(define three (mcons (mcons 'x '()) (mcons 'y '())))

(define x (mcons 'x '()))
(define four (mcons x (mcons 'y x)))
(count-pairs four)

(define y (mcons x x))
(define seven (mcons y y))
(count-pairs seven)

(define inf (mlist 'x 'y 'z))
(set-mcdr! (mcdr (mcdr inf)) inf)
(count-pairs inf)
