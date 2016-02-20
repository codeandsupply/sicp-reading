;; 3.3 Modeling with Mutable Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In addition to constructors and selectors, compound data objects
;; with mutable state require mutators.

;; 3.3.1 Mutable List Structures

;; Primary mutators for pairs are set-car! and set-cdr!
;;     - We can almost represent pairs, with `cons` using these
;;     - We would only need an additional `get-new-pair` constructor

;; Sharing and identity are complicated in a world with set-car! and
;; set-cdr! Since cons allows lists to reference the same pairs,
;; setting on z1 and z2 have different results:

(require scheme/mpair)

(define x (mlist 'a 'b))
(define z1 (mcons x x))
(define z2 (mcons (mlist 'a 'b) (mlist 'a 'b)))

(define (set-to-wow x)
  (set-mcar! (mcar x) 'wow)
  x)

(set-to-wow z1)
(set-to-wow z2)

;; 3.3.2 Representing Queues

;; We can build a queue with `set-car! and `set-cdr!`

;; We need the following procedures
;;     - make-queue      = constructor
;;     - empty-queue?    = selector
;;     - front-queue     = selector
;;     - insert-queue!   = mutator
;;     - delete-queue!   = mutator

;; Doing this with just cons/append would be inneficient because we
;; would need to scan the list each time. We can do better by
;; maintaining a front-pointer and rear-pointer.


(define front-ptr mcar)
(define rear-ptr mcdr)
(define set-front-ptr! set-mcar!)
(define set-rear-ptr! set-mcdr!)

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (mcons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error ("FRONT-QUEUE called with empty queue" queue))
      (mcar (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE-QUEUE! called with empty queue" queue))
        (else
         (set-front-ptr! queue (mcdr (front-ptr queue)))
         queue)))

;; 3.3.3 Representing Tables

(define (lookup key table)
  (let ((record (my-assoc key (mcdr table))))
    (if record
        (mcdr record)
        false)))
(define (my-assoc key records)
  (cond ((null? records) false)
        ((equal? (mcaar records) key) (mcar records))
        (else (my-assoc key (mcdr records)))))
(define (mcaar x) (mcar (mcar x)))

(define (insert! key value table)
  (let ((record (my-assoc key (mcdr table))))
    (if record
        (set-mcdr! record value)
        (set-mcdr! table (mcons (mcons key value) (mcdr table)))))
  'ok)

(define (make-table)
  (mlist '*table*))

;; We can make 2d tables as tables of tables

(define (lookup2 key-1 key-2 table)
  (let ((subtable (my-assoc key-1 (mcdr table))))
    (if subtable
        (let ((record (my-assoc key-2 (mcdr subtable))))
          (if record
              (mcdr record)
              false))
        false)))

(define (insert2! key-1 key-2 value table)
  (let ((subtable (my-assoc key-1 (mcdr table))))
    (if subtable
        (let ((record (my-assoc key-2 (mcdr subtable))))
          (if record
              (set-mcdr! record value)
              (set-mcdr! subtable
                        (cons (cons key-2 value)
                              (mcdr subtable)))))
        (set-mcdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (mcdr table)))))
  'ok)

;; 3.3.4 A simulation for digital circuits

;; Our primitives for building this simulation are:
;;
;; - Wires
;; - inverters
;; - and-gates
;; - or-gates

;; We're going to need the following primitives on wires:
;;
;;     (make-wire)
;;     (get-signal <wire>)
;;     (set-signal! <wire>)
;;     (add-action! <wire> <new value>)

;; With these we can define inverters, and the and-gate

;; To model this, we're going to register a function on an input wire,
;; to be run whenever the signal on that wire changes.

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= (signal-value new-value)))
          (begin
            (set! signal-value new-value)
            (call-each action-procedures)
            'done)))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))


(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal o2) (get-signal o2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))
(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((or (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" s))))
(define (logical-or s1 s2)
  (cond ((or (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" s))))


(define a (make-wire))
(define b (make-wire))
(define c (make-wire))

(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

;; We could make a half adder like this:
;;
;;     (or-gate a b d)
;;     (and-gate a b c)
;;     (inverter c e)
;;     (and-gate d e s)

;; Or we could name the function that does it, like this:
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c1 c-out)
    'ok))
