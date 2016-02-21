;; Exercise 3.40.  Give all possible values of x that can result from
;; executing

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

;; Which of these possibilities remain if we instead use serialized
;; procedures:

(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))


;; Answer
;;;;;;;;;

;; A
;;;;;

;; P1 then P2: 1,000,000
;; P2 then P1: 1,000,000
;; P1 read x -> P2 set x to 1000 -> P1 read x -> P1 set x to 10,000
;; P2 read x -> P1 set x to 100 -> P2 read x twice -> P2 set x to 100,000
;; P2 read x twice -> P1 set x to 100 -> P2 read x -> P2 set x to 10,000


;; B
;;;;;

;; Only 1,000,000, since both P1 then P2 will end the same
