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
