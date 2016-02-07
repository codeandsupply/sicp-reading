;; Exercise 3.19.  Redo exercise 3.18 using an algorithm that takes
;; only a constant amount of space. (This requires a very clever
;; idea.)

(require scheme/mpair)

(define (has-cycle-helper slow fast)
  (let ((next-slow (mcdr slow))
        (step-fast (mcdr fast)))
    (if (null? step-fast)
        #f
        (let ((next-fast (mcdr step-fast)))
          (cond ((null? next-fast) #f)
                ((eq? next-slow next-fast) #t)
                (else (has-cycle-helper next-slow next-fast)))))))

(define (has-cycle x)
  (has-cycle-helper x x))


(define x (mcons 'x '()))
(define four (mcons x (mcons 'y x)))
(has-cycle four)

(define y (mcons x x))
(define seven (mcons y y))
(has-cycle seven)

(define inf (mlist 'x 'y 'z))
(set-mcdr! (mcdr (mcdr inf)) inf)
(has-cycle inf)

