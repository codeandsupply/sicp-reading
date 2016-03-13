;; Exercise 4.14.  Eva Lu Ator and Louis Reasoner are each
;; experimenting with the metacircular evaluator. Eva types in the
;; definition of map, and runs some test programs that use it. They
;; work fine. Louis, in contrast, has installed the system version of
;; map as a primitive for the metacircular evaluator. When he tries
;; it, things go terribly wrong. Explain why Louis's map fails even
;; though Eva's works.

;; Answer
;;;;;;;;;

(define (map f l) (if (null? l) '() (cons (f (car l)) (map f (cdr l)))))
(map (lambda (x) (* x x)) '(1 2 3))

;; In order for map to work properly, it needs to understand its
;; arguments. In the metacircular evaluator, it isn't passed an actual
;; procedure, but rather a `compound-procedure` tagged object, and so
;; it blows up.

;; By defining it in our metacircular evaluator, it knows how to
;; handle its arguments.

;; We could install map as a "primitive" procedure, but it would need
;; to know how to take a `compound-procedure` object in addition to a
;; list.
