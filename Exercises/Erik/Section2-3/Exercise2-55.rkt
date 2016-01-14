#lang planet neil/sicp

;; Exercise 2.55. Eva Lu Ator types to the interpreter the expression

;;    (car ''abracadabra)

;; To her surprise, the interpreter prints back quote. Explain.

(car ''abracadabra)

;; Guess: This simply expands to `(quote (quote abracadabra))`. The
;; first quote causes the second to be interpreted as a symbol, which
;; is what is retured when you `car` it.
