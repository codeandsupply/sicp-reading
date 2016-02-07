;; Exercise 3.22.  Instead of representing a queue as a pair of
;; pointers, we can build a queue as a procedure with local state. The
;; local state will consist of pointers to the beginning and the end
;; of an ordinary list. Thus, the make-queue procedure will have the
;; form
;;
;;     (define (make-queue)
;;       (let ((front-ptr ...)
;;             (rear-ptr ...))
;;         <definitions of internal procedures>
;;         (define (dispatch m) ...)
;;         dispatch))
;;
;; Complete the definition of make-queue and provide implementations
;; of the queue operations using this representation.

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty?)
      (null? front-ptr))
    (define (front)
      (if (empty?)
          (error "FRONT called with empty queue" front-ptr)
          (mcar front-ptr)))
    (define (insert! item)
      (let ((new-pair (mcons item '())))
        (cond ((empty?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               'ok)
              (else
               (set-mcdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               'ok))))
    (define (delete!)
      (cond ((empty?) (error "DELETE called with empty queue" frt-ptr))
            (else
             (set! front-ptr (mcdr front-ptr))
             'ok)))
    (define (dispatch m)
      (cond ((eq? m 'empty?) empty?)
            ((eq? m 'front) front)
            ((eq? m 'insert!) insert!)
            ((eq? m 'delete!) delete!)
            (else (error "Invalide call. Undefined QUEUE method" m))))
    dispatch))


(define q (make-queue))
((q 'empty?))
((q 'insert!) 'a)
((q 'front))
((q 'insert!) 'b)
((q 'insert!) 'c)
((q 'front))
((q 'delete!))
((q 'front))
