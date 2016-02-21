;; Exercise 3.38.  Suppose that Peter, Paul, and Mary share a joint
;; bank account that initially contains $100. Concurrently, Peter
;; deposits $10, Paul withdraws $20, and Mary withdraws half the money
;; in the account, by executing the following commands:

;; Peter:	(set! balance (+ balance 10))
;; Paul:	(set! balance (- balance 20))
;; Mary:	(set! balance (- balance (/ balance 2)))

;; a. List all the different possible values for balance after these
;; three transactions have been completed, assuming that the banking
;; system forces the three processes to run sequentially in some
;; order.

;; b. What are some other values that could be produced if the system
;; allows the processes to be interleaved? Draw timing diagrams like
;; the one in figure 3.29 to explain how these values can occur.


;; Answer
;;;;;;;;;

;; A
;;;;;

;; Peter -> Paul -> Mary: 45
;; Peter -> Mary -> Paul: 35
;; Paul -> Mary -> Peter: 50
;; Paul -> Peter -> Mary: 45
;; Mary -> Peter -> Paul: 40
;; Mary -> Paul -> Peter: 40

;; B
;;;;;

;; I think these are the highest and lowest it can get:

;; 110:
;;   - Peter read / calculate (+ balance 10)
;;   - Paul & Mary execute fully
;;   - Peter (set! balance 110)

;; 30:
;;   - Mary read / calculate (/ balance 2)
;;   - Paul read / calculate (- balance 20)
;;   - Peter execute fully
;;   - Paul (set! balance 80)
;;   - Mary read / calculate (- balance 50)
;;   - Mary (set! balance 30)

;; 25:
;;   - Paul read / calculate (- balance 20)
;;   - Peter execute fully
;;   - Mary read / calculate (/ balance 2)
;;   - Paul (set! balance 80)
;;   - Mary read / calculate (- balance 55)
;;   - Mary (set! balance 25)
