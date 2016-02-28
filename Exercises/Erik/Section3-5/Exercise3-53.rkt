;; Exercise 3.53.  Without running the program, describe the elements
;; of the stream defined by

(define s (stream-cons 1 (add-streams s s)))

;; 1, 2, 4, 8, 16
