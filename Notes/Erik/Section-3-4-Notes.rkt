;; Implementing serializers

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'aquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

;; This function is defined in terms of mutexes. It returns a function
;; that takes in a procedure, and returns a wrapper around that
;; procedure.
;;
;; Using the same serializer on multiple procedures results in them
;; trying to access the same mutex.

(define (make-mutex)
  (let ((cell (mlist false)))
    (define (the-mutex m)
      (cond ((eq? m 'aquire)
             (if (test-and-set! cell)
                 'ok
                 (the-mutex 'aquire))) ;; retry
            ((eq? m 'release) (clear! cell))
            (else (error "Unexpected method -- MUTEX" m))))
    the-mutex))

(define (clear! cell)
  (set-mcar! cell false)
  'ok)

(define (test-and-set! cell)
  ;; Note, for this to work, it needs to be performed atomically.
  (if (mcar cell)
      true
      (begin (set-mcar! cell true)
             false)))
